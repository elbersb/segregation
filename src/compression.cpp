// [[Rcpp::depends(RcppProgress)]]
#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
using namespace Rcpp;

struct CompressionResults
{
    std::vector<int> iter;
    std::vector<double> M;
    std::vector<double> M_wgt;
    std::vector<double> N_units;
    std::vector<std::string> old_unit;
    std::vector<std::string> new_unit;
};

std::tuple<double, double> calculate_m(std::map<std::string, std::vector<double>> &data)
{
    // create group sums
    int n_groups = data.begin()->second.size();
    std::vector<double> group_sums(n_groups, 0.0);
    for (auto &[unit, counts] : data)
    {
        for (int i = 0; i < n_groups; i++)
            group_sums[i] += counts[i];
    }

    double n_total = std::accumulate(group_sums.begin(), group_sums.end(), 0);

    double m_total = 0.0;
    for (auto &[unit, counts] : data)
    {
        double n_unit = std::accumulate(counts.begin(), counts.end(), 0);
        for (int i = 0; i < n_groups; i++)
        {
            double obs = counts[i] / n_total;
            if (obs == 0)
                continue;
            double exp = (n_unit / n_total * group_sums[i] / n_total);
            m_total += obs * std::log(obs / exp);
        }
    }

    return std::make_tuple(n_total, m_total);
}

double calculate_reduction(double n, std::vector<double> &unit1, std::vector<double> &unit2)
{
    // create group sums
    const int n_groups = unit1.size();
    double n_total = 0.0;
    std::vector<double> group_sums(n_groups, 0.0);
    for (int i = 0; i < n_groups; i++)
    {
        group_sums[i] += unit1[i];
        group_sums[i] += unit2[i];
        n_total += unit1[i];
        n_total += unit2[i];
    }

    // create unit sums
    const double n_unit1 = std::accumulate(unit1.begin(), unit1.end(), 0);
    const double n_unit2 = std::accumulate(unit2.begin(), unit2.end(), 0);

    // calculate M
    double m_total = 0.0;
    for (int i = 0; i < n_groups; i++)
    {
        // unit 1
        double obs1 = unit1[i] / n_total;
        if (obs1 != 0)
        {
            double exp1 = (n_unit1 / n_total * group_sums[i] / n_total);
            m_total += obs1 * std::log(obs1 / exp1);
        }
        // unit 2
        double obs2 = unit2[i] / n_total;
        if (obs2 != 0)
        {
            double exp2 = (n_unit2 / n_total * group_sums[i] / n_total);
            m_total += obs2 * std::log(obs2 / exp2);
        }
    }

    return n_total / n * m_total;
}

// [[Rcpp::export]]
List compress_compute_cpp(
    std::string neighbors_option,
    StringMatrix m_neighbors,
    NumericMatrix m_data,
    std::vector<std::string> unit_names,
    int max_iter)
{
    // prepare main data structure: map, where the key is the unit name
    // and the values are the ordered group counts
    std::map<std::string, std::vector<double>> data;
    for (int i = 0; i < m_data.nrow(); i++)
    {
        auto unit = unit_names[i];
        data[unit] = {};
        for (int j = 0; j < m_data.ncol(); j++)
            data[unit].push_back(m_data(i, j));
    }

    int n_groups = m_data.ncol();
    // compute total M index
    double n_total, m_total;
    std::tie(n_total, m_total) = calculate_m(data);

    // prepare neighbors data structure (list of sets with 2 elements each)
    std::map<std::set<std::string>, double> neighbors;
    if (neighbors_option == "all")
    {
        for (int row = 0; row < unit_names.size(); row++)
        {
            for (int col = row + 1; col < unit_names.size(); col++)
            {
                neighbors[{unit_names[row], unit_names[col]}] = 0;
            }
        }
    }
    else if (neighbors_option == "df" || neighbors_option == "local")
    {
        for (int i = 0; i < m_neighbors.nrow(); i++)
        {
            std::string unit1 = Rcpp::as<std::string>(m_neighbors(i, 0));
            std::string unit2 = Rcpp::as<std::string>(m_neighbors(i, 1));
            if (unit1 != unit2)
                neighbors[{unit1, unit2}] = 0;
        }
    }

    // calculate reduction for each neighbor pair
    // (we don't do this in the previous step because otherwise we
    // might do a lot of duplicate calculations)
    for (const auto &[key, reduction] : neighbors)
    {
        auto iter = key.begin();
        const std::string unit1 = *iter;
        const std::string unit2 = *next(iter);
        neighbors[{unit1, unit2}] = calculate_reduction(n_total, data[unit1], data[unit2]);
    }

    // determine maximum number of iterations
    if (max_iter == -1)
    {
        max_iter = std::min(static_cast<int>(neighbors.size()), static_cast<int>(unit_names.size()) - 1);
    }

    CompressionResults results;
    results.iter.reserve(max_iter);
    results.M_wgt.reserve(max_iter);
    results.M.reserve(max_iter);
    results.N_units.reserve(max_iter);
    results.old_unit.reserve(max_iter);
    results.new_unit.reserve(max_iter);

    int counter = 0;
    double m_current = m_total;
    int n_units_current = m_data.nrow();

    Progress p(n_units_current, true);
    while (neighbors.size() > 0)
    {
        if (Progress::check_abort())
            return List::create();

        // find smallest reduction
        double min_reduction = 10000;
        std::set<std::string> min_key;
        for (const auto &[key, reduction] : neighbors)
        {
            if (reduction < min_reduction)
            {
                min_reduction = reduction;
                min_key = key;
                if (reduction == 0)
                    break;
            }
        }

        auto iter = min_key.begin();
        const std::string unit_keep = *iter;
        const std::string unit_delete = *next(iter);
        // add counts of 'delete' to 'keep', delete unit
        for (int i = 0; i < n_groups; i++)
            data[unit_keep][i] += data[unit_delete][i];
        data.erase(unit_delete);

        // update neighbors
        std::vector<std::set<std::string>> delete_neighbors;
        std::map<std::set<std::string>, double> new_neighbors;
        for (const auto &[key, reduction] : neighbors)
        {
            const bool delete_found = key.find(unit_delete) != key.end();
            const bool keep_found = key.find(unit_keep) != key.end();
            if (keep_found && delete_found)
            {
                // remove the neighbor pair
                delete_neighbors.push_back(key);
            }
            else if (delete_found)
            {
                // replace deleted unit with new unit
                delete_neighbors.push_back(key);
                std::set<std::string> new_key(key);
                new_key.erase(unit_delete);
                new_key.insert(unit_keep);

                auto iter = new_key.begin();
                const std::string unit1 = *iter;
                const std::string unit2 = *next(iter);
                new_neighbors[{unit1, unit2}] = calculate_reduction(n_total, data[unit1], data[unit2]);
            }
            else if (keep_found)
            {
                // recalculate if updated unit is involved
                auto iter = key.begin();
                const std::string unit1 = *iter;
                const std::string unit2 = *next(iter);
                new_neighbors[{unit1, unit2}] = calculate_reduction(n_total, data[unit1], data[unit2]);
            }
        }
        // delete neighbors that involve the old unit
        for (int i = 0; i < delete_neighbors.size(); i++)
            neighbors.erase(delete_neighbors[i]);

        // update and add new neighbors
        for (const auto &[key, reduction] : new_neighbors)
            neighbors[key] = reduction;

        // update results
        m_current -= min_reduction;
        n_units_current -= 1;
        counter += 1;
        results.iter.push_back(counter);
        results.M_wgt.push_back(min_reduction);
        results.M.push_back(m_current);
        results.N_units.push_back(n_units_current);
        results.old_unit.push_back(unit_delete);
        results.new_unit.push_back(unit_keep);

        if (counter == max_iter)
            break;

        p.increment();
    }

    return List::create(
        _["iter"] = results.iter,
        _["M_wgt"] = results.M_wgt,
        _["M"] = results.M,
        _["N_units"] = results.N_units,
        _["old_unit"] = results.old_unit,
        _["new_unit"] = results.new_unit);
}

int find_in_sets(String needle, std::vector<std::set<String>> haystack)
{
    for (int i = 0; i < haystack.size(); i++)
    {
        const bool is_in = haystack[i].find(needle) != haystack[i].end();
        if (is_in == true)
            return i;
    }
    return -1;
}

// [[Rcpp::export]]
List get_crosswalk_cpp(StringVector old_unit, StringVector new_unit)
{
    std::vector<std::set<String>> bags;

    for (int i = 0; i < old_unit.size(); i++)
    {
        int old_unit_bag = find_in_sets(old_unit[i], bags);
        int new_unit_bag = find_in_sets(new_unit[i], bags);

        if (old_unit_bag == -1 && new_unit_bag == -1)
        {
            // neither unit in bags - add new bag
            bags.push_back({old_unit[i], new_unit[i]});
        }
        else if (old_unit_bag != -1 && new_unit_bag == -1)
        {
            // old_unit already exists - add new_unit to same bag
            bags[old_unit_bag].insert(new_unit[i]);
        }
        else if (old_unit_bag == -1 && new_unit_bag != -1)
        {
            // new_unit already exists - add old_unit to same bag
            bags[new_unit_bag].insert(old_unit[i]);
        }
        else if (old_unit_bag != -1 && new_unit_bag != -1)
        {
            // both units exist in different bags, merge the two
            bags[old_unit_bag].insert(bags[new_unit_bag].begin(), bags[new_unit_bag].end());
            bags.erase(bags.begin() + new_unit_bag);
        }
    }

    // convert to List
    List l(bags.size());
    for (int i = 0; i < bags.size(); i++)
    {
        StringVector bag(bags[i].size());
        int index = 0;
        for (auto el : bags[i])
        {
            bag[index] = el;
            index++;
        }
        l[i] = bag;
    }
    return l;
}