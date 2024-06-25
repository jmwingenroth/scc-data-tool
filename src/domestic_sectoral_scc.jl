# Set up environment and model parameters

using Pkg
Pkg.activate(".")
Pkg.instantiate()

using Mimi, MimiGIVE, DataFrames

scc_sectoral_domestic = function(year, n)

    scc_dir = "output/scghg/scghg-$year-n$n"
    mkpath(scc_dir)

    discount_rates = [
        (label = "1.5%", prtp = exp(9.149606e-05) - 1, eta = 1.016010e+00), 
        (label = "2.0%", prtp = exp(0.001972641) - 1, eta = 1.244458999), 
        (label = "2.5%", prtp = exp(0.004618784) - 1, eta = 1.421158088), 
        (label = "3.0%", prtp = exp(0.007702711) - 1, eta = 1.567899391),
    ];

    pricelevel_2005_to_2020 = 113.648 / 87.504

    save_list = [
        (:Socioeconomic, :population_global),     # Global population (millions of persons)
        (:PerCapitaGDP,  :global_pc_gdp),         # Global per capita GDP (thousands of USD $2005/yr)
        (:Socioeconomic, :co2_emissions),         # Emissions (GtC/yr)
        (:Socioeconomic, :n2o_emissions),         # Emissions (GtN2O/yr)
        (:Socioeconomic, :ch4_emissions),         # Emissions (GtCH4/yr)
        (:TempNorm_1850to1900, :global_temperature_norm), # Global surface temperature anomaly (K) from preinudstrial
        (:global_sea_level, :sea_level_rise),     # Total sea level rise from all components (includes landwater storage for projection periods) (m)
        (:OceanPH, :pH),                          # Ocean pH levels
        (:co2_cycle, :co2),                       # Total atmospheric concentrations (ppm)
        (:n2o_cycle, :N₂O),                       # Total atmospheric concentrations (ppb)
        (:ch4_cycle, :CH₄)                        # Total atmospheric concentrations (ppb)
    ];

    covar_dir = "output/covariates/covariates-n$n"

    m = MimiGIVE.get_model(socioeconomics_source=:RFF)

    # Compute SCC values

    println("Running year = $year")

    results = MimiGIVE.compute_scc(m;
        year = year,
        last_year = 2300,
        discount_rates = discount_rates,
        certainty_equivalent = true,
        fair_parameter_set = :random,
        rffsp_sampling = :random,
        n = n,
        gas = :CO2,
        save_list = save_list,
        output_dir = covar_dir,
        save_md = true,
        save_cpc = true,
        compute_sectoral_values = true,
        compute_domestic_values = true,
        CIAM_foresight = :perfect,
        CIAM_GDPcap = true,
        pulse_size = 1e-4)

    ## blank data
    scghg = DataFrame(sector=Symbol[], discount_rate=String[], scghg=Float64[])
    ce_scghg = DataFrame(sector=Symbol[], discount_rate=String[], ce_scghg=Float64[])

    ## populate data
    for (k,v) in results[:scc]
        push!(ce_scghg, (sector=k.sector, discount_rate=k.dr_label, ce_scghg=results[:scc][k].ce_scc*pricelevel_2005_to_2020))
        for sc in results[:scc][k].sccs
            push!(scghg, (sector=k.sector, discount_rate=k.dr_label, scghg=sc*pricelevel_2005_to_2020))
        end
    end
    
    ## export    
    scghg |> save(joinpath(scc_dir, "scc-n$n.csv"))
    ce_scghg |> save(joinpath(scc_dir, "cert-equiv-scc-n$n.csv"))

    #marginal damages
    for (k,v) in results[:mds]
        DataFrame(v, :auto) |> save(joinpath(scc_dir, "mds_n$n-$(k.sector).csv"))
    end    

    #CPC
    for (k,v) in results[:cpc]
        DataFrame(v, :auto) |> save(joinpath(scc_dir, "cpc_n$n-$(k.sector).csv"))
    end    
    
    nothing
end
