# Set up parallel environment and model parameters

using Pkg
Pkg.activate(".")
Pkg.instantiate()

using Distributed

addprocs(4-nprocs())

@everywhere using Pkg
@everywhere Pkg.activate(".")
@everywhere using Mimi, MimiGIVE, DataFrames, Random

@everywhere n = 3

@everywhere discount_rates = [
    (label="2.0% CDR", prtp=0.02, eta=0.),
    (label="3.0% CDR", prtp=0.03, eta=0.), 
    (label="2.0% Ramsey", prtp=exp(0.00197263996888441)-1, eta=1.24445906630114),
    (label="3.0% Ramsey", prtp=exp(0.00770271075587262)-1, eta=1.56789939457574)
];

socioeconomics=[:RFF, :SSP1, :SSP2, :SSP3, :SSP5];
sectors = [:sectoral_and_dice, :h_and_s];
years = 2020:10:2100;
gases = [:CO2, :CH4, :N2O];

@everywhere m_RFF = MimiGIVE.get_model(socioeconomics_source=:RFF)
@everywhere m_SSP1 = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP = "SSP1", RCP = "RCP2.6")
@everywhere m_SSP2 = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP = "SSP2", RCP = "RCP4.5")
@everywhere m_SSP3 = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP = "SSP3", RCP = "RCP7.0")
@everywhere m_SSP5 = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP = "SSP5", RCP = "RCP8.5")

# Compute SCC values

@time res = pmap((gas, year, sector, socioeconomic) for gas in gases, year in years, sector in sectors, socioeconomic in socioeconomics) do (gas, year, sector, socioeconomic)
    
    println("Running socio: $socioeconomic, sector: $sector, pulse year: $year, gas: $gas")

    output_dir = "output/scghg/scghg-$socioeconomic-$sector-$year-$gas-n$n"
    mkpath(output_dir)

    if socioeconomic == :RFF
        m = m_RFF
    elseif socioeconomic == :SSP1
        m = m_SSP1
    elseif socioeconomic == :SSP2
        m = m_SSP2
    elseif socioeconomic == :SSP3
        m = m_SSP3
    elseif socioeconomic == :SSP5
        m = m_SSP5
    else
        error("Socioeconomics source $socioeconomic doesn't match available options: $socioeconomics")
    end
    
    if sector == :sectoral_and_dice
        update_param!(m, :DamageAggregator, :include_cromar_mortality,  true)
        update_param!(m, :DamageAggregator, :include_ag,                true)
        update_param!(m, :DamageAggregator, :include_slr,               true)
        update_param!(m, :DamageAggregator, :include_energy,            true)
        update_param!(m, :DamageAggregator, :include_dice2016R2,        true)
        update_param!(m, :DamageAggregator, :include_hs_damage,         false)
    elseif sector == :h_and_s
        update_param!(m, :DamageAggregator, :include_cromar_mortality,  false)
        update_param!(m, :DamageAggregator, :include_ag,                false)
        update_param!(m, :DamageAggregator, :include_slr,               false)
        update_param!(m, :DamageAggregator, :include_energy,            false)
        update_param!(m, :DamageAggregator, :include_dice2016R2,        false)
        update_param!(m, :DamageAggregator, :include_hs_damage,         true)
    else
        error("Damage scheme $sector doesn't match available options: $sectors")
    end

    results = MimiGIVE.compute_scc(m,
                                   n = n,
                                   year = year,
                                   gas = gas,
                                   discount_rates = discount_rates,
                                   save_md = true,
                                   save_cpc = true,
                                   compute_sectoral_values = true,
                                   certainty_equivalent = true)

    ## blank data
    scghg = DataFrame(sector=Symbol[], discount_rate=String[], scghg=Float64[])
    
    ## populate data
    for (k,v) in results[:scc]
        for sc in results[:scc][k].sccs
            push!(scghg, (sector=k.sector, discount_rate=k.dr_label, scghg=sc))
        end
    end
    
    ## export    
    scghg |> save(joinpath(output_dir, "sc-$gas-n$n.csv"))

    #marginal damages
    for (k,v) in results[:mds]
        DataFrame(v, :auto) |> save(joinpath(output_dir, "mds_$gas--n$n-$(k.sector).csv"))
    end    

end

# Set up for Monte Carlo run for covariates

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
