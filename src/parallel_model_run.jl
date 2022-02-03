using Pkg
Pkg.instantiate()

using Mimi, MimiGIVE, DataFrames, Random, Distributed;

addprocs(4-nprocs())

@everywhere using Pkg
@everywhere Pkg.activate("")
@everywhere using Mimi, MimiGIVE, DataFrames, Random

@everywhere n = 10

@everywhere discount_rates = [
    (label="1.0% CDR", prtp=0.01, eta=0.),
    (label="1.5% CDR", prtp=0.015, eta=0.),
    (label="2.0% CDR", prtp=0.02, eta=0.),
    (label="2.5% CDR", prtp=0.025, eta=0.),
    (label="3.0% CDR", prtp=0.03, eta=0.), 
    (label="1.0% Ramsey", prtp=0., eta=0.67),
    (label="1.5% Ramsey", prtp=exp(0.000091496076937417)-1, eta=1.01601025469387),
    (label="2.0% Ramsey", prtp=exp(0.00197263996888441)-1, eta=1.24445906630114),
    (label="2.5% Ramsey", prtp=exp(0.00461878398879138)-1, eta=1.42115811609748),
    (label="3.0% Ramsey", prtp=exp(0.00770271075587262)-1, eta=1.56789939457574)
];

@everywhere save_list = [(:Socioeconomic, :population_global),     # Global population (millions of persons)
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

@everywhere m_RFF = MimiGIVE.get_model(socioeconomics_source=:RFF)

@everywhere m_SSP2 = MimiGIVE.get_model(socioeconomics_source=:SSP)

@time res = pmap((gas) for gas in [:CO2, :N2O, :CH4]) do (gas)
    
    update_param!(m_SSP2, :DamageAggregator, :include_dice2016R2, true)

    output_dir = "output/mcs-$gas-n$n"
    mkpath(output_dir)

    results = MimiGIVE.compute_scc(m_SSP2,
                                   n = n,
                                   year = 2020,
                                   gas = gas,
                                   discount_rates = discount_rates,
                                   drop_rffsp_outliers = false,
                                   save_md = true,
                                   save_list = save_list,
                                   output_dir = output_dir,
                                   compute_sectoral_values = true)

    ## blank data
    scghg = DataFrame(sector=Symbol[], discount_rate=String[], scghg=Float64[])
    
    ## populate data
    for (k,v) in results[:scc]
        for sc in results[:scc][k].sccs
            push!(scghg, (sector=k.sector, discount_rate=k.dr_label, scghg=sc))
        end
    end
    
    ## export    
    scghg |> save(joinpath(output_dir, "sc-$gas--n$n--.csv"))

    #marginal damages
    for (k,v) in results[:mds]
        DataFrame(v, :auto) |> save(joinpath(output_dir, "mds_$gas--n$n-$(k.sector).csv"))
    end    

end
