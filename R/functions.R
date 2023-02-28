## -------------------------------------------------- ##
#                Wrangle raw TRY data
## -------------------------------------------------- ##

wrangle_TRY_qual_func <- function(TRYdata){
  
  # Main Request - Initial Tweaks -----
  
  # Perform initial wrangling
  TRYdata2 <- TRYdata %>%
    # Filter to the traits we want
    dplyr::filter(TraitID %in% c(205, 213, 915, 99, 59, 347, 343, 335, 29, 26, 2807)) %>%
    # Drop duplicate rows
    dplyr::distinct() %>%
    # select only relevant columns
    dplyr::select(AccSpeciesName, TraitName, DataName, OrigValueStr, OrigUnitStr) %>%
    # also fix some funny characters in OrigValueStr
    dplyr::mutate(OrigValueStr = case_when(
      OrigValueStr == "disk flowers with nectar \xb1 hidden in centre of flower" ~ "disk flowers with nectar hidden in centre of flower",
      OrigValueStr == "poly-annuals < 5 years (short-lived perennials)" ~ "poly-annuals less than 5 years (short-lived perennials)",
      OrigValueStr == "other|polygamodioecy" ~ "other/polygamodioecy",
      OrigValueStr == "long lived perennial >= 50 yrs." ~ "long lived perennial greater than or equal 50 yrs.",
      TRUE ~ OrigValueStr)) %>%
    # filter out the rows with nothing in OrigValueStr
    dplyr::filter(nchar(OrigValueStr) != 0) %>%
    # simplify species name
    tidyr::separate(col = AccSpeciesName,
                    into = c("genus", "epithet", "extra"),
                    fill = "right", extra = "merge", sep = " ") %>%
    # reassemble species name and drop "extra" column
    dplyr::mutate(species = paste(genus, epithet), .before = everything()) %>%
    dplyr::select(-extra) %>%
    # convert month numbers to characters
    dplyr::mutate(OrigValueStr = case_when(
      OrigUnitStr == "month" & OrigValueStr == "1" ~ "Jan",
      OrigUnitStr == "month" & OrigValueStr == "2" ~ "Feb",
      OrigUnitStr == "month" & OrigValueStr == "3" ~ "Mar",
      OrigUnitStr == "month" & OrigValueStr == "4" ~ "Apr",
      OrigUnitStr == "month" & OrigValueStr == "5" ~ "May",
      OrigUnitStr == "month" & OrigValueStr == "6" ~ "Jun",
      OrigUnitStr == "month" & OrigValueStr == "7" ~ "Jul",
      OrigUnitStr == "month" & OrigValueStr == "8" ~ "Aug",
      OrigUnitStr == "month" & OrigValueStr == "9" ~ "Sep",
      OrigUnitStr == "month" & OrigValueStr == "10" ~ "Oct",
      OrigUnitStr == "month" & OrigValueStr == "11" ~ "Nov",
      OrigUnitStr == "month" & OrigValueStr == "12" ~ "Dec",
      TRUE ~ OrigValueStr))
  
  # Take a quick look at this object
  # dplyr::glimpse(TRYdata2)
  
  
  # split TRY data into a dataframe where OrigValueStr consists of all letters
  TRYdata3A <- TRYdata2 %>%
    dplyr::filter(suppressWarnings(is.na(as.numeric(OrigValueStr))) == TRUE) %>%
    dplyr::mutate(OrigValueStr = tolower(OrigValueStr)) %>%
    unique()
  
  # Main Request - Characters ----
  
  # Look at data names
  # unique(TRYdata3A$DataName)
  
  # Standardize this object
  TRYdata3A_v2 <- TRYdata3A %>%
    # filter out the "not applicable" and "not available" values
    dplyr::filter(!tolower(OrigValueStr) %in% c("not applicable", "not available") & 
                    !DataName %in% c("Plant phenology: Annual", "Plant phenology: Biennial", "Plant phenology: Perennial")) %>%
    # fix the trait names
    dplyr::mutate(trait_fixed = dplyr::case_when(
      DataName %in% c("Plant life form (Raunkiaer life form)") ~ "plant_life_form",
      DataName %in% c("Plant life span") ~ "plant_lifespan",
      DataName %in% c("Onset of flowering (first flowering date, beginning of flowering period)") ~ "flowering_begin",
      DataName %in% c("End of flowering") ~ "flowering_end",
      DataName %in% c("Flower sexual system", "Dicliny (monoeceous, dioecious, hermaphrodite)") ~ "flower_sexual_system",
      DataName %in% c("Apomixis") ~ "apomixis",
      DataName %in% c("Flower: pollinator and type of reward", "Pollination syndrome (pollen vector)", "Pollination syndrom 2") ~ "pollinator",
      DataName %in% c("Flower type") ~ "flower_type",
      DataName %in% c("Fruit type") ~ "fruit_type",
      DataName %in% c("Fruit/Seed Period Begin", "Seed shedding season (time of seed dispersal)") ~ "fruit_seed_begin",
      DataName %in% c("Fruit/Seed Period End") ~ "fruit_seed_end",
      DataName %in% c("Fruit/Seed Persistence") ~ "fruit_seed_persist",
      DataName %in% c("Plant lifespan, longevity, plant maximum age", "Perennation 1 (plant age)", "Perenniality") ~ "perenniality",
      DataName %in% c("Flowering season", "Flowering Period Length (duration of flowering period)", "Flowering periode: peak month") ~ "flowering_season",
      DataName %in% c("Time (season) of germination (seedling emergence)") ~ "germination_season"
    ), .before = DataName) %>%
    dplyr::select(-DataName, -TraitName) %>%
    # make the values and units into lowercase
    dplyr::mutate(OrigValueStr = tolower(OrigValueStr),
                  OrigUnitStr = tolower(OrigUnitStr)) %>%
    unique() %>%
    # fix the trait units
    dplyr::mutate(trait_units = ifelse(test = nchar(OrigUnitStr) == 0 |
                                         is.na(OrigValueStr), 
                                       yes = trait_fixed,
                                       no = paste0(trait_fixed, "_", OrigUnitStr))) %>%
    dplyr::select(-OrigUnitStr, -trait_fixed, -genus, -epithet) %>%
    # collapse the values into one string for each species/trait combo
    dplyr::group_by(species, trait_units) %>%
    dplyr::summarize(value_entry = paste(OrigValueStr, collapse ="; ")) %>%
    # Ungroup
    dplyr::ungroup()
  
  # checking to make sure everything looks ok
  # glimpse(TRYdata3A_v2)
  # unique(TRYdata3A_v2$trait_units)
  
  # convert to wide format
  TRYdata3A_wide <- TRYdata3A_v2 %>%
    pivot_wider(names_from = "trait_units", values_from = "value_entry")
  
  # export csv
  write.csv(x = TRYdata3A_wide, row.names = F, na = '',
            file = file.path("trait_files", "Copy of TRYdata_qualitative_wide.csv"))
  
  # upload csv to google drive
  googledrive::drive_upload(media = file.path("trait_files", "Copy of TRYdata_qualitative_wide.csv"), 
                            name = "Copy of TRYdata_qualitative_wide.csv",
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1O9ddBUHniErr-VkUwfYb6R_B9wdh9jmf"),
                            overwrite = T)
  
  return(file.path("trait_files", "Copy of TRYdata_qualitative_wide.csv"))
  
}


wrangle_TRY_quant_func <- function(TRYdata){
  # Main Request - Initial Tweaks -----
  
  # Perform initial wrangling
  TRYdata2 <- TRYdata %>%
    # Filter to the traits we want
    dplyr::filter(TraitID %in% c(205, 213, 915, 99, 59, 347, 343, 335, 29, 26, 2807)) %>%
    # Drop duplicate rows
    dplyr::distinct() %>%
    # select only relevant columns
    dplyr::select(AccSpeciesName, TraitName, DataName, OrigValueStr, OrigUnitStr) %>%
    # also fix some funny characters in OrigValueStr
    dplyr::mutate(OrigValueStr = case_when(
      OrigValueStr == "disk flowers with nectar \xb1 hidden in centre of flower" ~ "disk flowers with nectar hidden in centre of flower",
      OrigValueStr == "poly-annuals < 5 years (short-lived perennials)" ~ "poly-annuals less than 5 years (short-lived perennials)",
      OrigValueStr == "other|polygamodioecy" ~ "other/polygamodioecy",
      OrigValueStr == "long lived perennial >= 50 yrs." ~ "long lived perennial greater than or equal 50 yrs.",
      TRUE ~ OrigValueStr)) %>%
    # filter out the rows with nothing in OrigValueStr
    dplyr::filter(nchar(OrigValueStr) != 0) %>%
    # simplify species name
    tidyr::separate(col = AccSpeciesName,
                    into = c("genus", "epithet", "extra"),
                    fill = "right", extra = "merge", sep = " ") %>%
    # reassemble species name and drop "extra" column
    dplyr::mutate(species = paste(genus, epithet), .before = everything()) %>%
    dplyr::select(-extra) %>%
    # convert month numbers to characters
    dplyr::mutate(OrigValueStr = case_when(
      OrigUnitStr == "month" & OrigValueStr == "1" ~ "Jan",
      OrigUnitStr == "month" & OrigValueStr == "2" ~ "Feb",
      OrigUnitStr == "month" & OrigValueStr == "3" ~ "Mar",
      OrigUnitStr == "month" & OrigValueStr == "4" ~ "Apr",
      OrigUnitStr == "month" & OrigValueStr == "5" ~ "May",
      OrigUnitStr == "month" & OrigValueStr == "6" ~ "Jun",
      OrigUnitStr == "month" & OrigValueStr == "7" ~ "Jul",
      OrigUnitStr == "month" & OrigValueStr == "8" ~ "Aug",
      OrigUnitStr == "month" & OrigValueStr == "9" ~ "Sep",
      OrigUnitStr == "month" & OrigValueStr == "10" ~ "Oct",
      OrigUnitStr == "month" & OrigValueStr == "11" ~ "Nov",
      OrigUnitStr == "month" & OrigValueStr == "12" ~ "Dec",
      TRUE ~ OrigValueStr))
  
  # Take a quick look at this object
  # dplyr::glimpse(TRYdata2)
  
  
  # split TRY data into a dataframe where OrigValueStr consists of all numbers
  TRYdata3B <- TRYdata2 %>%
    dplyr::filter(suppressWarnings(is.na(as.numeric(OrigValueStr))) == FALSE)
  
  
  
  # Main Request - Numbers ----
  
  # Tweak as needed
  TRYdata3B_v2 <- TRYdata3B %>%
    dplyr::select(-genus, -epithet) %>%
    # filter out the "extreme" values
    dplyr::filter(DataName != "Plant lifespan (longevity) extreme") %>%
    # convert OrigValueStr to numeric so we can do some math on it
    dplyr::mutate(OrigValueStr = as.numeric(OrigValueStr)) %>%
    # fix the trait names
    dplyr::mutate(trait_fixed = dplyr::case_when(
      DataName %in% c("Plant lifespan (longevity) min", "Plant lifespan (longevity) max", "Plant lifespan, longevity, plant maximum age", 
                      "Plant life span", "Plant longevity") ~ "plant_lifespan",
      DataName %in% c("Seed mass original value: min", "Seed mass original value: max", "Seed mass min", "Seed mass max", 
                      "Seed mass", "Seed mass original value: mean") ~ "seed_mass",
      DataName %in% c("Seed dry mass") ~ "seed_dry_mass",
      DataName %in% c("Onset of flowering (first flowering date, beginning of flowering period)") ~ "flowering_begin",
      DataName %in% c("Flowering season") ~ "flowering_season",
      DataName %in% c("Plant life form (Raunkiaer life form)") ~ "plant_life_form"
      
    ), .before = DataName) %>%
    dplyr::select(-TraitName, -DataName) %>%
    # convert the trait units
    dplyr::mutate(converted_value = dplyr::case_when(
      OrigUnitStr == "mg" ~ OrigValueStr/1000,
      OrigUnitStr == "gr" ~ OrigValueStr,
      OrigUnitStr == "g / 1000 seeds" ~ OrigValueStr/1000,
      OrigUnitStr %in% c("1/lb", "1/pound") ~ OrigValueStr*2.2,
      TRUE ~ OrigValueStr)) %>%
    # consolidate the trait units
    dplyr::mutate(converted_unit = dplyr::case_when(
      OrigUnitStr %in% c("mg", "gr", "g / 1000 seeds") ~ "g",
      OrigUnitStr %in% c("years", "yr", "yrs") ~ "year",
      OrigUnitStr %in% c("1/lb", "1/pound", "1/kg") ~ "1_per_kg",
      OrigUnitStr == "doy" ~ "day_of_year",
      TRUE ~ OrigUnitStr)) %>%
    select(-starts_with("Orig")) %>%
    # attach the name of the unit to the trait name
    dplyr::mutate(trait_units = paste0(trait_fixed, "_", converted_unit)) %>%
    dplyr::select(-trait_fixed, -converted_unit) %>%
    # calculate the mean across all species/trait combos
    dplyr::group_by(species, trait_units) %>%
    dplyr::summarize(value_entry = mean(converted_value, na.rm = T)) %>%
    dplyr::ungroup()
  
  # checking to make sure everything looks ok
  # dplyr::glimpse(TRYdata3B_v2)
  
  # convert to wide format
  TRYdata3B_wide <- TRYdata3B_v2 %>%
    tidyr::pivot_wider(names_from = "trait_units",
                       values_from = "value_entry")
  
  # export csv
  write.csv(x = TRYdata3B_wide, row.names = F, na = "",
            file = file.path("trait_files", "Copy of TRYdata_quantitative_wide.csv"))
  
  # upload csv to google drive
  googledrive::drive_upload(media = file.path("trait_files", "Copy of TRYdata_quantitative_wide.csv"),
                            name = "Copy of TRYdata_quantitative_wide.csv",
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1O9ddBUHniErr-VkUwfYb6R_B9wdh9jmf"),
                            overwrite = T)
  
  return(file.path("trait_files", "Copy of TRYdata_quantitative_wide.csv"))
  
}

## -------------------------------------------------- ##
#        Combine Attributes Table & TRY Data
## -------------------------------------------------- ##

integrate_TRY_USDA_func <- function(usda, try_qual, try_quant){
  
  # Prep for integration ----

  # Define undesireable species
  undesirables <- c("Abies balsamea", "Acer negundo", "Acer pensylvanicum", "Betula lenta", "Betula papyrifera", "Carpinus caroliniana", "Casearia guianensis", "Cercis canadensis", "Frangula caroliniana", "Hamamelis virginiana", "Liquidambar styraciflua", "Ostrya virginiana", "Picea rubens", "Pinus contorta", "Pinus flexilis", "Pinus rigida", "Pinus strobus", "Piper glabrescens", "Platanus occidentalis", "Prunus virginiana", "Quercus coccinea", "Quercus falcata", "Quercus michauxii", "Quercus velutina", "Sambucus canadensis", "Sassafras albidum", "Solanum rugosum", "Sorbus americana", "Thuja occidentalis", "Ulmus americana", "Viburnum lantanoides", "Viburnum nudum")
  
  # Standardize
  try_qual_v2 <- try_qual %>%
    # Drop unwanted species
    dplyr::filter(!species %in% undesirables) %>%
    # Pivot long
    tidyr::pivot_longer(col = -species,
                        names_to = "columns",
                        values_to = "entries") %>%
    # Add good info to columns
    dplyr::mutate(columns = paste0("TRY_qual_", columns)) %>%
    # Pivot back to wide format
    tidyr::pivot_wider(names_from = columns,
                       values_from = entries) %>%
    # Make REF columns
    dplyr::mutate(TRY_qual_flowering_season_REF = ifelse(
      test = nchar(TRY_qual_flowering_season) == 0,
      yes = NA, no = "TRY_Kattge_etal_2020"), 
      .after = TRY_qual_flowering_season) %>%
    dplyr::mutate(TRY_qual_fruit_seed_persist_REF = ifelse(
      test = nchar(TRY_qual_fruit_seed_persist) == 0,
      yes = NA, no = "TRY_Kattge_etal_2020"), 
      .after = TRY_qual_fruit_seed_persist)
  
  # Process
  try_quant_v2 <- try_quant %>%
    # Drop unwanted species
    dplyr::filter(!species %in% undesirables) %>%
    # Pivot long
    tidyr::pivot_longer(col = -species,
                        names_to = "columns",
                        values_to = "entries") %>%
    # Fix a weird issue with some of the col names
    dplyr::mutate(columns = dplyr::case_when(
      columns == "plant_life_form_" ~ "plant_life_form",
      columns == "plant_lifespan_" ~ "plant_lifespan",
      TRUE ~ columns)) %>%
    # Add good info to columns
    dplyr::mutate(columns = paste0("TRY_quant_", columns)) %>%
    # Pivot back to wide format
    tidyr::pivot_wider(names_from = columns,
                       values_from = entries) %>%
    # Make REF columns
    dplyr::mutate(TRY_quant_seed_dry_mass_REF = ifelse(
      test = nchar(TRY_quant_seed_dry_mass_g) == 0,
      yes = NA, no = "TRY_Kattge_etal_2020"), .after = TRY_quant_seed_dry_mass_g) %>%
    dplyr::mutate(TRY_quant_plant_lifespan_year_REF = ifelse(
      test = nchar(TRY_quant_plant_lifespan_year) == 0,
      yes = NA, no = "TRY_Kattge_etal_2020"), .after = TRY_quant_plant_lifespan_year)
  
  
  # Integrate! ----

  # Combine them!
  all_traits_v1 <- usda %>%
    # Attach qualitative information
    dplyr::left_join(try_qual_v2, by = "species") %>%
    # Attach quantitative too
    dplyr::left_join(try_quant_v2, by = "species")
  
  # Take a lightning fast look
  # dplyr::glimpse(all_traits_v1)
  
  
  # Further Collapse Traits ----

  
  # Final standardization
  all_traits <- all_traits_v1 %>%
    # Filter to only spp in data
    dplyr::filter(included_in_data == 1) %>%
    # Do special unit conversions
    dplyr::mutate(TRY_quant_seed_dry_mass_mg = TRY_quant_seed_dry_mass_g * 1000, .before = TRY_quant_seed_dry_mass_g) %>%
    # Drop unwanted values
    dplyr::select(-included_in_data, -TRY_quant_plant_lifespan, -TRY_quant_flowering_season_month, -TRY_quant_seed_mass_g,	-TRY_quant_plant_life_form, -TRY_quant_seed_dry_mass_g, -TRY_quant_flowering_season_day_of_year, -TRY_quant_flowering_begin_month, -TRY_quant_seed_dry_mass_1_per_kg,	-TRY_quant_seed_mass_1_per_kg, -TRY_qual_fruit_type, -TRY_qual_flowering_begin_month, -TRY_qual_flower_type, -TRY_qual_apomixis, -TRY_qual_germination_season, -TRY_qual_flowering_end_month,	-TRY_qual_flowering_season_month, -TRY_quant_seed_mass_1_per_kg, -TRY_quant_seed_dry_mass_1_per_kg, -TRY_qual_plant_life_form, -TRY_qual_flowering_end_month, -TRY_qual_flowering_season_month, -TRY_qual_germination_season, -TRY_qual_apomixis, -TRY_qual_flower_type, -TRY_qual_flowering_begin_month, -TRY_qual_fruit_type, -TRY_qual_perenniality, -TRY_qual_plant_lifespan, -TRY_qual_fruit_seed_begin, -TRY_qual_fruit_seed_end, -Seeds_per_lb, -Seeds_per_lb_REF, -TRY_qual_flower_sexual_system) %>%
    # Fix the seedbank (seed persistence) issue
    dplyr::mutate(TRY_qual_fruit_seed_persist = ifelse(
      test = species %in% c("Cecropia schreberiana", "Picea mariana", "Pinus contorta", "Pinus rigida", "Prunus virginiana", "Amelanchier arborea", "Acer rubrum", "Betula alleghanienesis", "Betula papyrifera", "Nyssa sylvatica", "Robinia pseudoacacia", "Sassafras albidum"),
      yes = "yes", no = TRY_qual_fruit_seed_persist)) %>%
    # Coalesce columns
    dplyr::mutate(
      trait_seed_mass_mg = coalesce(TRY_quant_seed_dry_mass_mg, as.numeric(Seed_mass_mg)),
      trait_lifespan_years = coalesce(TRY_quant_plant_lifespan_year, Lifespan_yrs),
      trait_flowering_season = coalesce(Flowering_season, TRY_qual_flowering_season),
      trait_fruit_seed_persist = coalesce(Fruit_seed_persist, TRY_qual_fruit_seed_persist)) %>%
    # Coalesce the REF columns
    dplyr::mutate(
      trait_seed_mass_mg_REF = coalesce(TRY_quant_seed_dry_mass_REF, Seed_mass_REF),
      trait_lifespan_years_REF = coalesce(TRY_quant_plant_lifespan_year_REF, Lifespan_yrs_REF),
      trait_flowering_season_REF = coalesce(Flowering_season_REF, TRY_qual_flowering_season_REF),
      trait_fruit_seed_persist_REF = coalesce(Fruit_seed_persist_REF, TRY_qual_fruit_seed_persist_REF)) %>%
    # Rename some columns
    dplyr::rename(Sexual_system = Monoecious_or_Dioecious,
                  Sexual_system_REF = Monoecious.or.Dioecious_REF) %>%
    # Drop coalesced columns
    dplyr::select(-TRY_quant_seed_dry_mass_mg, -Seed_mass_mg, -TRY_quant_plant_lifespan_year, -Lifespan_yrs, -TRY_qual_pollinator, 
                  -Flowering_season, -TRY_qual_flowering_season, -Fruit_seed_persist, -TRY_qual_fruit_seed_persist,
                  -TRY_quant_seed_dry_mass_REF, -Lifespan_yrs_REF, -TRY_quant_plant_lifespan_year_REF, -Seed_mass_REF, 
                  -Flowering_season_REF, -TRY_qual_flowering_season_REF, -Fruit_seed_persist_REF, -TRY_qual_fruit_seed_persist_REF) %>%
    # Even more fine-tuning after office hours on Oct 19, Oct 26, Nov 9 2022
    # Rename some columns
    dplyr::rename(AND = Andrews,
                  ADK = Adirondack,
                  CDR = CedarCreek,
                  HBR = HubbardBrook,
                  CWT = Coweeta,
                  HFR = Harvard,
                  BNZ = Bonanza,
                  SEV = Sevilleta,
                  LUQ = Luquillo,
                  Seed_development_1_2or3yrs = Seed_development_2yrsOR3yrs,
                  Seed_development_1_2or3yrs_REF = Seed_development_2yrsOR3yrs_REF,
                  Growth_form = GrowthForm,
                  Growth_form_REF = GrowthForm_REF,
                  Growth_habit = Growth.Habit,
                  Seed_mass_mg = trait_seed_mass_mg,
                  Lifespan_years = trait_lifespan_years,
                  Flowering_season = trait_flowering_season,
                  Seed_bank = trait_fruit_seed_persist,
                  Seed_mass_REF = trait_seed_mass_mg_REF,
                  Lifespan_years_REF = trait_lifespan_years_REF,
                  Flowering_season_REF = trait_flowering_season_REF,
                  Seed_bank_REF = trait_fruit_seed_persist_REF) %>%
    # Making a REF column that was missing from the LTER_Attributes_USDA_Oct2022 googlesheet
    dplyr::mutate(Growth_habit_REF = ifelse(
      test = nchar(Growth_habit) == 0,
      yes = NA, no = "Pearse, I (pers comm)"), .after = Growth_habit) %>%
    # Fine-tuning the REF column even more
    dplyr::mutate(Growth_habit_REF = ifelse(
      test = species == "Clusia gundlachii" | species == "Clusia rosea",
      yes = "Zimmerman (pers comm)", no = Growth_habit_REF)) %>%
    # Relocating some REF columns
    dplyr::relocate(Lifespan_years, .after = Seed_Maturation_Phenology_REF) %>%
    dplyr::relocate(Lifespan_years_REF, .after = Lifespan_years) %>%
    dplyr::relocate(Seed_mass_mg, .after = Lifespan_years_REF) %>%
    dplyr::relocate(Seed_mass_REF, .after = Seed_mass_mg) %>%
    dplyr::relocate(Flowering_season_REF, .after = Flowering_season) %>%
    dplyr::relocate(Seed_bank_REF, .after = Seed_bank) %>%
    dplyr::relocate(Fleshy_fruit, .after = Seed_bank_REF) %>%
    dplyr::relocate(Fleshy_fruit_REF, .after = Fleshy_fruit) %>%
    dplyr::relocate(Dispersal_syndrome, .after = Fleshy_fruit_REF) %>%
    dplyr::relocate(Dispersal_syndrome_REF, .after = Dispersal_syndrome) %>%
    # If "wind" is detected in Pollinator_vector, then we put "wind" in Pollinator_code. Otherwise, we put "animal"
    dplyr::mutate(Pollinator_code = ifelse(
      test = str_detect(Pollinator_vector, "wind"),
      yes = "wind", no = "animal"), .after = Pollinator_vector) %>%
    # If "wind" is detected in Seed_dispersal_vector, then we put "wind" in Seed_dispersal_code. If it's "water" then we put "water". Otherwise, we put "animal"
    dplyr::mutate(Seed_dispersal_code = case_when(
      str_detect(Seed_dispersal_vector, "wind") ~ "wind",
      Seed_dispersal_vector == "water" ~ "water", 
      TRUE ~ "animal"), .after = Seed_dispersal_vector) %>%
    # Capitalize the values in Shade_tolerance
    dplyr::mutate(Shade_tolerance = str_to_title(Shade_tolerance)) %>%
    # Replace the blank values in Flowering_season with NA's
    dplyr::mutate(Flowering_season = ifelse(
      test = nchar(Flowering_season) == 0,
      yes = NA, no = Flowering_season)) %>%
    # Add some common names
    dplyr::mutate(common = ifelse(
      test = species == "Quercus ellipsoidalis",
      yes = "northern pin oak", no = common)) %>%
    dplyr::mutate(common = ifelse(
      test = species == "Quercus macrocarpa",
      yes = "bur oak", no = common))
  
  # Glimpse it
  # dplyr::glimpse(all_traits)
  
  # Export ----

  # Name exported file
  (file_name <- paste0("LTER_integrated_attributes_USDA_", Sys.Date(), ".csv"))
  
  # Write the CSV
  write.csv(x = all_traits, row.names = F, na = "",
            file.path("trait_files", file_name))
  
  # And upload to google!
  googledrive::drive_upload(media = file.path("trait_files", file_name),
                            name = file_name, overwrite = T,
                            path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1O9ddBUHniErr-VkUwfYb6R_B9wdh9jmf"))
  
  return(file.path("trait_files", file_name))
  # End ----
  
}

## -------------------------------------------------- ##
#    Calculating & Exporting Stats for Attributes Table
## -------------------------------------------------- ##

calculate_stats <- function(all_traits){
  # making a folder for our exported csvs
  export_folder <- paste0("export_stats_", Sys.Date())
  dir.create(path = file.path(export_folder), showWarnings = FALSE)
  
  # Calculating & Exporting Quantitative Stats ------

  # calculating overall quantitative stats 
  overall_stats_for_quant_atts <- all_traits %>% 
    get_summary_stats(
      Lifespan_years, Seed_mass_mg, 
      show = c("n", "min", "max", "mean", "sd"))
  
  # exporting overall quantitative stats 
  write.csv(overall_stats_for_quant_atts, file.path(export_folder, "overall_stats_for_quant_atts.csv"), row.names = FALSE)
  
  # listing the sites we want
  sites_we_want <- c("AND", "ADK", "CDR", "HBR",
                     "CWT", "HFR", "BNZ", "SEV", "LUQ")
  
  site_stats_for_quant_atts <- list()
  
  # calculating quantitative stats for each individual site
  for (site in sites_we_want){
    some_stats <-  all_traits %>% 
      filter(!!sym(site) == 1) %>%
      get_summary_stats(
        Lifespan_years, Seed_mass_mg, 
        show = c("n", "min", "max", "mean", "sd"))
    
    site_stats_for_quant_atts[[site]] <- some_stats
    
  }
  
  # adding a 'site' column and combining all the site dataframes into one dataframe
  site_stats_for_quant_atts_df <- site_stats_for_quant_atts %>%
    purrr::imap(.f = ~mutate(.x, site = paste0(.y), .before = everything())) %>%
    purrr::map_dfr(.f = select, everything())
  
  # exporting quantitative stats for each individual site
  write.csv(site_stats_for_quant_atts_df, file.path(export_folder, "site_stats_for_quant_atts.csv"), row.names = FALSE)
  
  # Calculating & Exporting Qualitative Stats ----------

  # listing the columns we want
  columns_we_want <- c("Seed_development_1_2or3yrs", "Seed_dispersal_vector",
                       "Seed_dispersal_code",
                       "Pollinator_vector", "Pollinator_code",
                       "Mycorrhiza_AM_EM", "Needleleaf_Broadleaf",
                       "Deciduous_Evergreen_yrs", "Seed_Maturation_Phenology",
                       "Seed_Maturation_Code", "Sexual_system",
                       "Shade_tolerance", "Growth_form",
                       "Growth_habit", "Flowering_season",
                       "Seed_bank", "Fleshy_fruit",
                       "Dispersal_syndrome")
  
  overall_stats_for_qual_atts <- list()
  
  # calculating overall qualitative stats 
  for (col in columns_we_want){
    some_stats <- all_traits %>%
      dplyr::group_by(!!sym(col)) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::mutate(variable = col, .before = everything()) %>%
      dplyr::rename(value = !!sym(col))
    
    overall_stats_for_qual_atts[[col]] <- some_stats
    
  }
  
  # combining overall qualitative stats into one dataframe
  overall_stats_for_qual_atts_df <- overall_stats_for_qual_atts %>%
    purrr::map_dfr(.f = select, everything())
  
  # exporting overall qualitative stats 
  write.csv(overall_stats_for_qual_atts_df, file.path(export_folder, "overall_stats_for_qual_atts.csv"), row.names = FALSE)
  
  
  site_stats_for_qual_atts <- list()
  
  # calculating qualitative stats for each individual site
  for (site in sites_we_want){
    for (col in columns_we_want){
      some_stats <- all_traits %>%
        dplyr::filter(!!sym(site) == 1) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::summarize(count = n()) %>%
        dplyr::mutate(variable = col, .before = everything()) %>%
        dplyr::rename(value = !!sym(col))
      
      site_stats_for_qual_atts[[site]][[col]] <- some_stats
    }
  }
  
  # adding a 'site' column 
  for (site in sites_we_want){
    site_stats_for_qual_atts[[site]] <- site_stats_for_qual_atts[[site]] %>% 
      purrr::map_dfr(.f = select, everything()) %>%
      dplyr::mutate(site = site, .before = everything())
  }
  
  # combining all the site dataframes into one dataframe
  site_stats_for_qual_atts_df <- site_stats_for_qual_atts %>%
    purrr::map_dfr(.f = select, everything())
  
  # exporting qualitative stats for each individual site
  write.csv(site_stats_for_qual_atts_df, file.path(export_folder, "site_stats_for_qual_atts.csv"), row.names = FALSE)
  
  # End ----
  return(export_folder)
}
