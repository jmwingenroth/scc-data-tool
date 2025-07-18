# Set up parallel environment and model parameters

using Pkg
Pkg.activate(".")
Pkg.instantiate()

using Distributed, Hwloc

addprocs(num_physical_cores() - nprocs())

@everywhere using Pkg
@everywhere Pkg.activate(".")
@everywhere using Mimi, MimiGIVE, DataFrames, Random, ProgressMeter

@everywhere n = 10_000

@everywhere discount_rates = [
    (label="2.0% CDR", prtp=0.02, eta=0.),
    (label="3.0% CDR", prtp=0.03, eta=0.), 
    (label="2.0% Ramsey", prtp=exp(0.00197263996888441)-1, eta=1.24445906630114),
    (label="3.0% Ramsey", prtp=exp(0.00770271075587262)-1, eta=1.56789939457574)
];

socioeconomics=[:RFF, :SSP1, :SSP2, :SSP3, :SSP5];
sectors = [:sectoral, :dice, :h_and_s];
years = 2020:10:2100;
gases = [:CO2, :CH4, :N2O];

@everywhere other_vars = [(:Socioeconomic, :population_global),     # Global population (millions of persons)
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

# Compute SCC values

@showprogress pmap((gas, year, sector, socioeconomic) for gas in gases, year in years, sector in sectors, socioeconomic in socioeconomics) do (gas, year, sector, socioeconomic)
    
    println("Running socio: $socioeconomic, sector: $sector, pulse year: $year, gas: $gas")

    scc_dir = "output/scghg/scghg-$socioeconomic-$sector-$year-$gas-n$n"
    mkpath(scc_dir)

    if socioeconomic == :RFF
        m = MimiGIVE.get_model(socioeconomics_source=:RFF)
    elseif socioeconomic == :SSP1
        m = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP_scenario = "SSP126")
    elseif socioeconomic == :SSP2
        m = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP_scenario = "SSP245")
    elseif socioeconomic == :SSP3
        m = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP_scenario = "SSP370")
    elseif socioeconomic == :SSP5
        m = MimiGIVE.get_model(socioeconomics_source=:SSP, SSP_scenario = "SSP585")

    else
        error("Socioeconomics source $socioeconomic doesn't match available options.")
    end
    
    if sector == :sectoral
        update_param!(m, :DamageAggregator, :include_cromar_mortality,  true)
        update_param!(m, :DamageAggregator, :include_ag,                true)
        update_param!(m, :DamageAggregator, :include_slr,               true)
        update_param!(m, :DamageAggregator, :include_energy,            true)
        update_param!(m, :DamageAggregator, :include_dice2016R2,        false)
        update_param!(m, :DamageAggregator, :include_hs_damage,         false)
        compute_sectoral_values = true
    elseif sector == :dice
        update_param!(m, :DamageAggregator, :include_cromar_mortality,  false)
        update_param!(m, :DamageAggregator, :include_ag,                false)
        update_param!(m, :DamageAggregator, :include_slr,               false)
        update_param!(m, :DamageAggregator, :include_energy,            false)
        update_param!(m, :DamageAggregator, :include_dice2016R2,        true)
        update_param!(m, :DamageAggregator, :include_hs_damage,         false)
        compute_sectoral_values = false
    elseif sector == :h_and_s
        update_param!(m, :DamageAggregator, :include_cromar_mortality,  false)
        update_param!(m, :DamageAggregator, :include_ag,                false)
        update_param!(m, :DamageAggregator, :include_slr,               false)
        update_param!(m, :DamageAggregator, :include_energy,            false)
        update_param!(m, :DamageAggregator, :include_dice2016R2,        false)
        update_param!(m, :DamageAggregator, :include_hs_damage,         true)
        compute_sectoral_values = false
    else
        error("Damage scheme $sector doesn't match available options.")
    end

    if (gas == gases[1]) & (year == years[1]) & (sector == sectors[1])
        save_list = other_vars
        covar_dir = "output/covariates/covariates-$socioeconomic-n$n"
    else
        save_list = []
        covar_dir = nothing
    end

    results = MimiGIVE.compute_scc(m;
        year = year,
        last_year = 2300,
        discount_rates = discount_rates,
        certainty_equivalent = true,
        fair_parameter_set = :random,
        rffsp_sampling = :random,
        n = n,
        gas = gas,
        save_list = save_list,
        output_dir = covar_dir,
        save_md = true,
        save_cpc = true,
        compute_sectoral_values = compute_sectoral_values,
        compute_domestic_values = false,
        CIAM_foresight = :perfect,
        CIAM_GDPcap = true,
        pulse_size = 1e-4)

    ## blank data
    scghg = DataFrame(sector=Symbol[], discount_rate=String[], scghg=Float64[])
    ce_scghg = DataFrame(sector=Symbol[], discount_rate=String[], ce_scghg=Float64[])

    ## populate data
    for (k,v) in results[:scc]
        push!(ce_scghg, (sector=k.sector, discount_rate=k.dr_label, ce_scghg=results[:scc][k].ce_scc))
        for sc in results[:scc][k].sccs
            push!(scghg, (sector=k.sector, discount_rate=k.dr_label, scghg=sc))
        end
    end
    
    ## export    
    scghg |> save(joinpath(scc_dir, "sc-$gas-n$n.csv"))
    ce_scghg |> save(joinpath(scc_dir, "cert-equiv-sc-$gas-n$n.csv"))

    #marginal damages
    for (k,v) in results[:mds]
        DataFrame(v, :auto) |> save(joinpath(scc_dir, "mds_$gas--n$n-$(k.sector).csv"))
    end    

    #CPC
    for (k,v) in results[:cpc]
        DataFrame(v, :auto) |> save(joinpath(scc_dir, "cpc_$gas--n$n-$(k.sector).csv"))
    end    
    
end
