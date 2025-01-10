### This code is to calculate species diversity indexes for Dangermond Preserve and surrounding areas
### Written by Erica Nielsen, 2024 using MARINe dataset from UCSC

# load libraries
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(maps)
library(viridis)


# first upload data and filter

#read point contact
pt_cont <- read_excel("~/cbs_data_CA_2023.xlsx", 
                      sheet = "point_contact_summary_data")

#read quadrat summary
quad <- read_excel("~/cbs_data_CA_2023.xlsx", 
                   sheet = "quadrat_summary_data")

#read swath data
swath <- read_excel("~/cbs_data_CA_2023.xlsx", 
                    sheet = "swath_summary_data")

#Here we remove species lumps ("spp"), and all other non-species-level data 
remove_chars = c("Rock", "Sand", "Tar", "Blue Green Algae", "Red Crust", "Diatom", "Ceramiales")

#only need to do for pt count and quadrat data
pt_cont_f <-pt_cont %>% filter(!(species_lump %in% remove_chars))
quad_f <-quad %>% filter(!(species_lump %in% remove_chars))


# edit tables so we have anything with total_count or number_of_hits > 0 is 1 (i.e. pres/abs)
# also filter to mainland (includes islands too tricky at this time)
pt_cont_P <- pt_cont_f %>%
  filter(island == "Mainland") %>%
  mutate(across(all_of('number_of_hits'), ~ ifelse(. > 0, 1, .)))
#rename so pres/abs col is called 'total_count' (so we can merge later)
names(pt_cont_P)[names(pt_cont_P) == "number_of_hits"] <- "total_count"

quad_P <- quad_f %>%
  filter(island == "Mainland") %>%
  mutate(across(all_of('total_count'), ~ ifelse(. > 0, 1, .)))

swath_P <- swath %>%
  filter(island == "Mainland") %>%
  mutate(across(all_of('total_count'), ~ ifelse(. > 0, 1, .)))

# now join dataframes to make single pres/abs spp table
common_col_names <- intersect(names(quad_P), names(swath_P)) #get column names
quad_swath_P<-merge(quad_P, swath_P, by=common_col_names, all.x=TRUE, all.y=TRUE) #merge first 2 DFs
common_col_names <- intersect(names(pt_cont_P), names(quad_swath_P)) #get column names
all_pres <- merge(pt_cont_P, quad_swath_P, by=common_col_names, all.x=TRUE, all.y=TRUE) #merge in swath
all_pres_f <-all_pres[,c(1,3,4,14,15,16)] #only include relevant columns

#group by site, lat, long, spp and make so if total_count>0 make 1
all_pres_site_spp <- all_pres_f %>%
  group_by(marine_site_name, latitude, longitude, species_lump) %>%
  summarize(num_count = sum(total_count > 0))

all_pres_noyear <- all_pres_site_spp %>%
  mutate(across(all_of('num_count'), ~ ifelse(. > 0, 1, .)))

#reshape so data is in longformat
pres.wide <- spread(all_pres_noyear, species_lump, num_count, fill = 0) # this isn't working, I think becuase spp names aren't unique (due to multiyear sampling)        

# Set the threshold value for column one - this is the 'buffer' around pt conception
# got from taking .5 deg lat on either side of Pt Conception lat
north_thres <- 34.4  # the northern lat that we count as part of Pt C
south_thres <- 34.9  # the southern lat that we count as part of Pt C

# Filter the dataframe so we only get sites that are south of Pt C
dist_Sbound <- pres.wide %>%
  filter(latitude < south_thres)

# filter out the lat/long columns (because we will filter all numeric columns now)
dist_Sbound <-dist_Sbound[,c(4:300)]

# Filter columns where the sum of all rows is zero (absent S of Pt C)
dist_S_abs <- dist_Sbound %>%
  select_if(function(col) sum(col) == 0)

# Save column names as a separate dataframe
S.abs.spp <- data.frame(ColumnNames = colnames(dist_S_abs))


# Filter within PT C range
dist_ptC <- pres.wide %>%
  filter(latitude >= south_thres, latitude <= north_thres)
dist_ptC <-dist_ptC[,c(4:300)]

#filter those to keep values greater than zero
dist_ptC_pres <- dist_ptC %>%
  select_if(function(col) sum(col) > 0)

ptC.pres.spp <- data.frame(ColumnNames = colnames(dist_ptC_pres))

# compare DFs to keep only spp with absent S of PT C and present in PT C (i.e. Southern range edge spp)

S.limit.spp <- merge(S.abs.spp, ptC.pres.spp, by = "ColumnNames", all = FALSE)



### Do the same for spp with Pt as their N range boundary

# Filter the dataframe so we only get sites that are N of Pt C
dist_Nbound <- pres.wide %>%
  filter(latitude > north_thres)

# filter out the lat/long columns (because we will filter all numeric columns now)
dist_Nbound <-dist_Nbound[,c(4:300)]

# Filter columns where the sum of all rows is zero
dist_N_abs <- dist_Nbound %>%
  select_if(function(col) sum(col) == 0)

# Get column names as a separate dataframe
N.abs.spp <- data.frame(ColumnNames = colnames(dist_N_abs))

#compare DFs to keep only spp with abs N of PT C and pres in PT C
N.limit.spp <- merge(N.abs.spp, ptC.pres.spp, by = "ColumnNames", all = FALSE) 


#### plot map of PT C threshold latitudes

states <- map_data("state")
wc <- states %>%
  filter(region %in% c("california"))

map1 <- ggplot() +
  geom_polygon(data = wc, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)  +
  geom_hline(yintercept = 34.9, color = "red")+
  geom_hline(yintercept = 34.4, color = "red")+theme(plot.background=element_blank(), panel.background=element_blank())

pdf("Pt.C.lat.lines.map.pdf")
map1
dev.off()

### make map of MARINe sample sites, colored by years sampled

#get state shapefile
states <- map_data("state")
wc <- states %>%
  filter(region %in% c("california"))

#make column stating # years sampled per site
## get the number of years sampled across sites  (using pt contact dataset)
yrs <- pt_cont_f %>% group_by(marine_site_name, latitude, longitude) %>% count(year, sort = TRUE)

yrs_cbs <-yrs %>%
  group_by(marine_site_name) %>%
  add_count(name = "num_years")

#make coords DF to plot
coords_cbs <- yrs_cbs[,c(2,3)]

#make map
map2 <- ggplot() +
  geom_polygon(data = wc, aes(x=long, y = lat, group = group), fill='lightgrey') + coord_fixed(1.3)  +
  geom_point(data=yrs_cbs, mapping = aes(x = longitude, y = latitude, color= num_years))+scale_color_viridis()+
  theme(plot.background=element_blank(), panel.background=element_blank(), axis.text=element_text(colour='black',size=14), axis.title.x=element_blank(), axis.title.y=element_blank())

lons = c(-126, -114)     # (max longitude, min longitude)
lats = c(32, 43)  

# Add scale and North arrow
site.yr.map <- map2 +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) + 
  coord_sf(xlim = lons, ylim = lats, crs = 4326) + 
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )

pdf("sample.site.yr.pdf")
site.yr.map
dev.off()

# plot zoomed in map of DP
map3 <- ggplot() +
  geom_polygon(data = wc, aes(x = long, y = lat, group = group), fill='lightgrey') +
  coord_map(xlim = c(-120.96,-119.44), ylim = c(34.35,35.32))+
  geom_point(data=yrs_cbs, mapping = aes(x = longitude, y = latitude, color= num_years))+scale_color_viridis()+
  theme(plot.background=element_blank(), panel.background=element_blank(), axis.text=element_text(colour='black',size=14), axis.title.x=element_blank(), axis.title.y=element_blank())


