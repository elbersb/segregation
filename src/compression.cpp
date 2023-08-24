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

std::tuple<double, double> calculate_m(std::map<std::string, std::vector<double>> data)
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

std::tuple<double, double> calculate_twounit_m(std::vector<double> &unit1, std::vector<double> &unit2)
{
    // create group sums
    int n_groups = unit1.size();
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
    double n_unit1 = std::accumulate(unit1.begin(), unit1.end(), 0);
    double n_unit2 = std::accumulate(unit2.begin(), unit2.end(), 0);

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

    return std::make_tuple(n_total, m_total);
}

// [[Rcpp::export]]
List compress_compute_cpp(
    StringMatrix m_neighbors,
    NumericMatrix m_data,
    StringVector unit_names,
    int max_iter)
{
    // prepare neighbors data structure (list of sets)
    std::vector<std::set<std::string>> neighbors;
    for (int i = 0; i < m_neighbors.nrow(); i++)
    {
        neighbors.push_back({});
        for (int j = 0; j < m_neighbors.ncol(); j++)
        {
            neighbors[neighbors.size() - 1].insert(Rcpp::as<std::string>(m_neighbors(i, j)));
        }
    }

    // prepare main data structure: map, where the key is the unit name
    // and the values are the ordered group counts
    std::map<std::string, std::vector<double>> data;
    for (int i = 0; i < m_data.nrow(); i++)
    {
        std::string unit = Rcpp::as<std::string>(unit_names[i]);
        data[unit] = {};
        for (int j = 0; j < m_data.ncol(); j++)
        {
            data[unit].push_back(m_data(i, j));
        }
    }

    int n_groups = m_data.ncol();

    // compute total M index
    double n_total, m_total;
    std::tie(n_total, m_total) = calculate_m(data);

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

        // analyze reductions for all neighbors
        double min_reduction = 10000;
        int min_index;

        for (int i = 0; i < neighbors.size(); i++)
        {
            auto iter = neighbors[i].begin();
            double n_total_pair, m_total_pair;
            std::tie(n_total_pair, m_total_pair) = calculate_twounit_m(
                data[*iter], data[*next(iter)]);
            // reduction = p_AB * M_AB
            double reduction = n_total_pair / n_total * m_total_pair;
            if (reduction < min_reduction)
            {
                min_reduction = reduction;
                min_index = i;
                if (reduction == 0)
                    break;
            }
        }

        auto iter = neighbors[min_index].begin();
        const std::string unit_keep = *iter;
        const std::string unit_delete = *next(iter);
        // add counts of second to first, delete second
        for (int i = 0; i < n_groups; i++)
        {
            data[unit_keep][i] += data[unit_delete][i];
        }
        data.erase(unit_delete);
        // update all neighbor references
        int mark_for_deletion;
        for (int i = 0; i < neighbors.size(); i++)
        {
            const bool is_in = neighbors[i].find(unit_delete) != neighbors[i].end();
            if (is_in)
            {
                neighbors[i].erase(unit_delete);
                neighbors[i].insert(unit_keep);
            }
            if (neighbors[i].size() == 1)
            {
                mark_for_deletion = i;
            }
        }
        // clean up neighbors: erase equal elements
        neighbors.erase(neighbors.begin() + mark_for_deletion);

        // clean up neighbors: erase duplicates
        std::sort(neighbors.begin(), neighbors.end());
        neighbors.erase(std::unique(neighbors.begin(), neighbors.end()), neighbors.end());

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
