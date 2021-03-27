#' @title prepTaxa: reshapes plant taxa lookup table
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#'
#' @description This function reshapes the plant taxa lookup table to match the tlu_Plants table in the original database
#' to make filtering and summarizing easier. Function is mostly internal and used for quadrat species, seedling and sapling summaries.
#'
#' @return Returns a dataframe with cover class midpoints for each quadrat and includes guild for each species.
#'
#' @examples
#' importData()
#' # reshape the plant taxa lookup to wide
#' taxa_wide <- prepTaxa()
#'
#' @export
#'

prepTaxa <- function(){

  env <- if(exists("VIEWS_NETN")){VIEWS_NETN} else {.GlobalEnv}

  tryCatch(taxa <- get("COMN_Taxa", envir = env) %>%
             select(TaxonID, TSN, ScientificName, CommonName, Order, Family,
                    Genus, Species, SubSpecies, IsExotic, InvasiveNETN, IsFernAlly,
                    TaxonGroupLabel, DeerIndicatorTree, DeerIndicatorHerb), # add FilterMIDN for MIDN
           error = function(e){stop("COMN_Taxa view not found. Please import view.")})

  #+++++++++++++++++++++++++
  # When the canopy exclusion column is added back, need to include in this function
  #+++++++++++++++++++++++++

  # Clean up taxa table so easier to work with
  names(taxa)[names(taxa) == "IsFernAlly"] <- "FernAlly"
  names(taxa)[names(taxa) == "IsExotic"] <- "Exotic"

  cols <- c("Order", "Family", "Genus", "Species", "SubSpecies")
  taxa[, cols] <- invisible(lapply(taxa[, cols], gsub, pattern = "NOT DETERMINED", replacement = NA))
  taxa$CommonName <- sub(",.*", "", taxa$CommonName)
  taxa$CommonName <- sub("/.*", "", taxa$CommonName)
  taxa$guild <- 1
  taxa$guild_text <- gsub("/", "", taxa$TaxonGroupLabel)

  # reshape guilds to wide
  taxa_wide <- taxa %>% select(-TaxonGroupLabel) %>%
    pivot_wider(names_from = guild_text,
                values_from = guild,
                values_fill = 0) %>%
    select(TaxonID, TSN, ScientificName, CommonName, Order, Family, Genus, Species, SubSpecies,
           Tree, TreeShrub, Shrub, Vine, Herbaceous, Graminoid, FernAlly, MossLichen, Exotic,
           InvasiveNETN, DeerIndicatorTree, DeerIndicatorHerb)

  # Clean this up after taxa table is fixed
  unk_aster <- data.frame(TaxonID = 1307, TSN = -9999999952, ScientificName = "Unknown Asteraceae - 01",
                          CommonName = "Unknown Asteraceae", Order = "Asterales", Family = 'Asteraceae',
                          Genus = NA, Species = NA, SubSpecies = NA, Tree = 0, TreeShrub = 0, Shrub = 0, Vine = 0,
                          Herbaceous = 1, Graminoid = 0, FernAlly = 0, MossLichen = 0, Exotic = 0,
                          InvasiveNETN = 0, DeerIndicatorTree = 0, DeerIndicatorHerb = 0)

  unk_symph <- data.frame(TaxonID = 1308, TSN = -9999999953, ScientificName = "Unknown Symphyotrichum - 01",
                          CommonName = "Unknown Asteraceae", Order = "Asterales", Family = 'Asteraceae',
                          Genus = "Symphyotrichum", Species = NA, SubSpecies = NA, Tree = 0,TreeShrub = 0, Shrub = 0, Vine = 0,
                          Herbaceous = 1, Graminoid = 0, FernAlly = 0, MossLichen = 0, Exotic = 0,
                          InvasiveNETN = 0, DeerIndicatorTree = 0, DeerIndicatorHerb = 0)

  unk_rubus <- data.frame(TaxonID = 1309, TSN = -9999999949, ScientificName = "Unknown Rubus - 01",
                          CommonName = "Unknown Rubus", Order = "Rosales", Family = 'Rosaceae',
                          Genus = "Rubus", Species = NA, SubSpecies = NA, Tree = 0,TreeShrub = 0, Shrub = 1, Vine = 0,
                          Herbaceous = 0, Graminoid = 0, FernAlly = 0, MossLichen = 0, Exotic = 0,
                          InvasiveNETN = 0, DeerIndicatorTree = 0, DeerIndicatorHerb = 0)

  taxa_wide <- rbind(taxa_wide, unk_aster, unk_symph, unk_rubus)

  return(taxa_wide)
}
