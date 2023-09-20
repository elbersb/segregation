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

std::vector<std::pair<std::string, double>> calculate_ls(std::map<std::string, std::vector<double>> &data)
{
    // create group sums
    int n_groups = data.begin()->second.size();
    std::vector<double> group_sums(n_groups, 0.0);
    for (auto &[unit, counts] : data)
    {
        for (int i = 0; i < n_groups; i++)
            group_sums[i] += counts[i];
    }

    // create group proportions
    double n_total = std::accumulate(group_sums.begin(), group_sums.end(), 0);
    std::vector<double> group_p(n_groups, 0.0);
    for (int i = 0; i < n_groups; i++)
    {
        group_p[i] = group_sums[i] / n_total;
    }

    // create local segregation scores for each unit
    std::vector<std::pair<std::string, double>> ls;
    for (auto &[unit, counts] : data)
    {
        double n_unit = std::accumulate(counts.begin(), counts.end(), 0);
        double ls_unit = 0.0;
        for (int i = 0; i < n_groups; i++)
        {
            double p_group_given_unit = counts[i] / n_unit;
            if (p_group_given_unit == 0)
                continue;
            ls_unit += p_group_given_unit * std::log(p_group_given_unit / group_p[i]);
        }
        ls.push_back({unit, ls_unit});
    }

    std::sort(ls.begin(), ls.end(), [](auto &left, auto &right)
              { return left.second < right.second; });

    return ls;
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

typedef std::pair<std::string, std::string> t_neighbor;

t_neighbor neighbor_make_pair(std::string a, std::string b)
{
    if (a < b)
        return t_neighbor(a, b);
    else
        return t_neighbor(b, a);
}

// [[Rcpp::export]]
List compress_compute_cpp(
    std::string neighbors_option,
    StringMatrix m_neighbors,
    int n_neighbors,
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

    // prepare neighbors data structure (list of pairs, where order is unimportant)
    std::map<t_neighbor, double> neighbors;
    if (neighbors_option == "all")
    {
        for (int row = 0; row < unit_names.size(); row++)
        {
            for (int col = row + 1; col < unit_names.size(); col++)
            {
                neighbors[neighbor_make_pair(unit_names[row], unit_names[col])] = 0;
            }
        }
    }
    else if (neighbors_option == "local")
    {

        auto ls = calculate_ls(data);
        for (int i = 0; i < ls.size(); i++)
        {
            for (int j = std::max(i - n_neighbors, 0);
                 j < std::min(i + n_neighbors + 1, static_cast<int>(ls.size()) - 1);
                 j++)
            {
                if (i != j)
                    neighbors[neighbor_make_pair(ls[i].first, ls[j].first)] = 0;
            }
        }
    }
    else if (neighbors_option == "df")
    {
        for (int i = 0; i < m_neighbors.nrow(); i++)
        {
            std::string unit1 = Rcpp::as<std::string>(m_neighbors(i, 0));
            std::string unit2 = Rcpp::as<std::string>(m_neighbors(i, 1));
            if (unit1 != unit2)
                neighbors[neighbor_make_pair(unit1, unit2)] = 0;
        }
    }

    // calculate reduction for each neighbor pair
    // (we don't do this in the previous step because otherwise we
    // might do a lot of duplicate calculations)
    for (const auto &[key, reduction] : neighbors)
    {
        neighbors[neighbor_make_pair(key.first, key.second)] = calculate_reduction(n_total, data[key.first], data[key.second]);
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
        t_neighbor min_key;
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

        const std::string unit_keep = min_key.first;
        const std::string unit_delete = min_key.second;
        // add counts of 'delete' to 'keep', delete unit
        for (int i = 0; i < n_groups; i++)
            data[unit_keep][i] += data[unit_delete][i];
        data.erase(unit_delete);

        // update neighbors
        neighbors.erase(min_key);

        std::vector<t_neighbor> delete_neighbors;
        std::map<t_neighbor, double> new_neighbors;
        for (const auto &[key, reduction] : neighbors)
        {
            // update pairs if deleted unit is involved
            if (key.first == unit_delete || key.second == unit_delete)
            {
                // this is a pair some_unit - deleted_unit -- replace deleted_unit with unit_keep
                delete_neighbors.push_back(key);
                std::string some_unit = (key.first == unit_delete) ? key.second : key.first;
                new_neighbors[neighbor_make_pair(unit_keep, some_unit)] =
                    calculate_reduction(n_total, data[unit_keep], data[some_unit]);
            }
            // recalculate reduction if kept unit is involved
            else if (key.first == unit_keep || key.second == unit_keep)
            {
                new_neighbors[key] = calculate_reduction(n_total, data[key.first], data[key.second]);
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
        if (n_units_current == 1)
        {
            // ensure that this is displayed as a true 0
            results.M.push_back(0);
        }
        else
        {
            results.M.push_back(m_current);
        }
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

int find_in_sets(std::string needle, std::vector<std::set<std::string>> haystack)
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
List get_crosswalk_cpp(std::vector<std::string> old_unit, std::vector<std::string> new_unit)
{
    std::vector<std::set<std::string>> bags;

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
        std::vector<std::string> bag(bags[i].size());
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