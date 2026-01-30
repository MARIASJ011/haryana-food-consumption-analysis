#-Codes
R 
#----------------------------------------# 
# Step 1: Load Required Libraries ---- 
#----------------------------------------# 
library(dplyr) 
library(ggplot2) 
library(sf) 
#----------------------------------------# 
# Step 2: Load Dataset ---- 
#----------------------------------------# 
# Set working directory where NSSO68.csv and GeoJSON are located 
setwd("D:\\R Assignments") 
# Read NSSO dataset 
df <- read.csv("D:\\Data\\NSSO68.csv") 
#----------------------------------------# 
# Step 3: Filter Data for Haryana ---- 
#----------------------------------------# 
# Filter rows where state_1 is 'HR' (Haryana) 
12 
hr <- df[df$state_1 == 'HR', ] 
dim(hr) 
# Convert District to factor (if needed) 
hr$District <- as.factor(hr$District) 
#----------------------------------------# 
# Step 4: Plot Histogram ---- 
#----------------------------------------# 
hist(hr$foodtotal_v, 
main = "Distribution of Food Consumption in Haryana", 
xlab = "Food Consumption (Rs.)", 
col = "lightgreen", 
border = "white") 
#----------------------------------------# 
# Step 5: Calculate District Averages ---- 
#----------------------------------------# 
# Summarise average food consumption per district 
hr_summary <- hr %>% 
group_by(District) %>% 
summarise(avg_food = mean(foodtotal_v, na.rm = TRUE)) %>% 
arrange(desc(avg_food)) 
# Optional: map District codes to names 
13 
district_map_hr <- data.frame( 
DistrictCode = sprintf("%02d", 1:21), 
DistrictName = c("Ambala", "Bhiwani", "Faridabad", "Fatehabad", "Gurgaon", "Hisar",  
"Jhajjar", "Jind", "Kaithal", "Karnal", "Kurukshetra", "Mahendragarh",  
"Mewat", "Palwal", "Panchkula", "Panipat", "Rewari", "Rohtak",  
"Sirsa", "Sonipat", "Yamunanagar")[1:21],  # Adjust as per dataset 
stringsAsFactors = FALSE 
) 
# Create DistrictCode for merging 
hr_summary$DistrictCode 
as.numeric(as.character(hr_summary$District))) 
# Merge district names 
<- 
hr_summary <- left_join(hr_summary, district_map_hr, by = "DistrictCode") 
#----------------------------------------# 
# Step 6: Barplot of Average Consumption ---- 
#----------------------------------------# 
barplot(height = hr_summary$avg_food, 
names.arg = hr_summary$DistrictName, 
las = 2, 
col = "skyblue", 
main = "Average Food Consumption by District (Haryana)", 
sprintf("%02d", 
ylab = "Average Food Consumption (Rs.)", 
14 
cex.names = 0.7) 
#----------------------------------------# 
# Step 7: Choropleth Map ---- 
#----------------------------------------# 
# Load Haryana GeoJSON file (must match district names) 
map_hr <- st_read("D:\\Data\\Haryana.geojson")   
# Rename district column if needed (adjust 'dtname' as per GeoJSON file) 
map_hr <- map_hr %>% 
rename(DistrictName = Dist_Name)   
# Merge spatial map with average food consumption data 
map_data_hr <- map_hr %>% 
left_join(hr_summary, by = "DistrictName") 
# Replace NAs with 0 
map_data_hr$avg_food[is.na(map_data_hr$avg_food)] <- 0 
# Plot the choropleth 
ggplot(map_data_hr) +  
geom_sf(aes(fill = avg_food, geometry = geometry)) +  
scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Avg Food (Rs.)") + 
15 
ggtitle("District-wise Average Food Consumption in Haryana") + 
theme_minimal() + 
geom_sf_text(aes(label = DistrictName), size = 3, color = "black") 
Python: 
#----------------------------------------# 
# Step 1: Load Required Libraries ---- 
#----------------------------------------# 
import pandas as pd 
import matplotlib.pyplot as plt 
import seaborn as sns 
import geopandas as gpd 
#----------------------------------------# 
# Step 2: Load Dataset ---- 
#----------------------------------------# 
# Load the NSSO dataset 
df = pd.read_csv("D:/Data/NSSO68.csv") 
#----------------------------------------# 
# Step 3: Filter Data for Haryana ---- 
#----------------------------------------# 
# Filter rows where state_1 is 'HR' 
hr = df[df['state_1'] == 'HR'].copy() 
16 
# Convert District to categorical (optional) 
hr['District'] = hr['District'].astype('category') 
print(f"Shape of Haryana data: {hr.shape}") 
#----------------------------------------# 
# Step 4: Plot Histogram ---- 
#----------------------------------------# 
plt.figure(figsize=(8, 5)) 
plt.hist(hr['foodtotal_v'].dropna(), bins=30, color='lightgreen', edgecolor='white') 
plt.title("Distribution of Food Consumption in Haryana") 
plt.xlabel("Food Consumption (Rs.)") 
plt.ylabel("Frequency") 
plt.tight_layout() 
plt.show() 
#----------------------------------------# 
# Step 5: Calculate District Averages ---- 
#----------------------------------------# 
# Group by District and calculate mean food consumption 
hr_summary = hr.groupby('District')['foodtotal_v'].mean().reset_index() 
hr_summary.columns = ['DistrictCode', 'avg_food'] 
hr_summary['DistrictCode'] = hr_summary['DistrictCode'].astype(int).apply(lambda x: 
f"{x:02}") 
17 
# Map DistrictCode to DistrictName (adjust names if needed) 
district_map_hr = pd.DataFrame({ 
'DistrictCode': [f"{i:02}" for i in range(1, 22)], 
'DistrictName': [ 
"Ambala", "Bhiwani", "Faridabad", "Fatehabad", "Gurgaon", "Hisar",  
"Jhajjar", "Jind", "Kaithal", "Karnal", "Kurukshetra", "Mahendragarh",  
"Mewat", "Palwal", "Panchkula", "Panipat", "Rewari", "Rohtak",  
"Sirsa", "Sonipat", "Yamunanagar" 
] 
}) 
# Merge average food with district names 
hr_summary = hr_summary.merge(district_map_hr, on='DistrictCode', how='left') 
#----------------------------------------# 
# Step 6: Barplot of Average Consumption ---- 
#----------------------------------------# 
plt.figure(figsize=(12, 6)) 
sns.barplot(x='DistrictName', y='avg_food', data=hr_summary, palette='Blues_d') 
plt.title("Average Food Consumption by District (Haryana)") 
plt.xlabel("District") 
plt.ylabel("Average Food Consumption (Rs.)") 
plt.xticks(rotation=45, ha='right') 
plt.tight_layout() 
18 
plt.show() 
#----------------------------------------# 
# Step 7: Choropleth Map ---- 
#----------------------------------------# 
# Load Haryana GeoJSON (make sure district names match) 
map_hr = gpd.read_file("D:/Data/Haryana.geojson") 
# Rename column if needed (e.g., 'Dist_Name' to 'DistrictName') 
map_hr = map_hr.rename(columns={'Dist_Name': 'DistrictName'}) 
# Merge spatial map with average food consumption data 
map_data_hr = map_hr.merge(hr_summary, on='DistrictName', how='left') 
# Replace NaNs with 0 in avg_food 
map_data_hr['avg_food'] = map_data_hr['avg_food'].fillna(0) 
# Plot Choropleth 
fig, ax = plt.subplots(1, 1, figsize=(10, 8)) 
map_data_hr.plot(column='avg_food', 
cmap='YlOrRd', 
linewidth=0.8, 
edgecolor='black', 
legend=True, 
19 
legend_kwds={'label': "Avg Food (Rs.)"}, 
ax=ax) 
# Add district names as labels (centroids) 
for idx, row in map_data_hr.iterrows(): 
if row['geometry'].centroid.is_empty: continue 
plt.text(row['geometry'].centroid.x,  
row['geometry'].centroid.y,  
row['DistrictName'],  
fontsize=7, ha='center', color='black') 
plt.title("District-wise Average Food Consumption in Haryana") 
plt.axis('off') 
plt.tight_layout() 
plt.show() 
