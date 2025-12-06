# 📦 Inventory Optimization Dashboard

**A Six Sigma Black Belt Project for Papemelroti**

![Status](https://img.shields.io/badge/Status-Complete-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)
![R](https://img.shields.io/badge/R-%3E%3D4.0-blue?logo=r)

---

## 🎯 Project Overview

This repository contains an **interactive Shiny dashboard** that demonstrates a data-driven inventory optimization system for Papemelroti, a Philippine retail company. The project applies **Six Sigma DMAIC methodology** combined with **Weighted Moving Average (WMA) forecasting** and **Inventory Turnover Optimization (ITO)** to improve inventory efficiency by **52.7%** while maintaining profitability.

### Key Results

| Metric | Baseline | Target | Improvement |
|--------|----------|--------|-------------|
| **Inventory Turnover Rate** | 2.62 | 4.00 | ↑ 52.7% |
| **Holding Period** | 139 days | 91 days | ↓ 34.5% |
| **Annual Savings** (5 stores) | — | ₱6,030 | — |
| **Sales Growth** | — | +5% | — |
| **Profit Margin** | 66.67% | 66.67% | ✓ Maintained |

---
### How to Use the Dashboard

Follow these simple steps to explore your inventory data:

#### **Step 1️⃣: Navigate to Data Analysis Tab**
- When the dashboard first loads, you'll see a welcome modal
- Click **"Let's Begin!"** to close the modal
- In the left sidebar menu, click on **"📊 Data Analysis"**
- You'll see a prompt to upload your CSV file

#### **Step 2️⃣: Upload Your CSV File**
- In the left sidebar under **"📁 Data Upload"**, click the **"Browse"** button
- Select your **`Papemelroti.csv`** file from your computer
- Wait for the confirmation message: **"✅ Successfully loaded [X] records"**
- The dashboard is now ready to analyze your data!

#### **Step 3️⃣: Navigate the Dashboard**
Once your data is loaded, you can explore any section using the left sidebar menu:

| Tab | What You'll See |
|-----|-----------------|
| **🎯 Key Results** | Executive summary with strategic visuals & QoI metrics |
| **⚠️ Problem Statement** | Business context & challenges Papemelroti faces |
| **🔧 Methodology** | WMA forecasting & ITO framework explanation |
| **💰 Financial Impact** | Cost savings breakdown & scalability analysis |
| **📊 Data Analysis** | Raw data, metrics table, forecast plots |
| **🔍 Root Cause Analysis** | 5 Whys investigation & Fault Tree diagram |
| **🎮 Interactive Simulator** | Adjust parameters to test inventory policies |
| **ℹ️ About & References** | Project info, team details, & citations |

#### **Bonus: Use the AI Assistant** 💬
- In the left sidebar, type a question in the **"🤖 AI Assistant"** text box
- Try questions like:
  - "What's the current IT rate?"
  - "What's the WMA forecast?"
  - "What are the projected savings?"
- Click **"Ask AI"** to get instant answers!

#### **Pro Tips** 🌟
- **Refresh Metrics:** Click the **"🔄 Refresh Metrics"** button to get real-time alerts on performance
- **View Data:** Start at the **"📊 Data Analysis"** tab to inspect your raw data
- **Understand Method:** Visit **"🔧 Methodology"** to learn how the system works
- **See Impact:** Check **"🎯 Key Results"** for the most important findings

## 🚀 Quick Start

### Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/your-org/papemelroti-inventory-optimization.git
   cd papemelroti-inventory-optimization
   ```

2. **Install R dependencies:**
   ```r
   # Open R console and run:
   packages <- c("shiny", "shinydashboard", "DiagrammeR", "dplyr", 
                 "tidyr", "ggplot2", "zoo", "visNetwork", "scales", 
                 "shinyalert", "DT")
   install.packages(packages)
   ```

3. **Run the application:**
   ```r
   shiny::runApp("shinyapp_final.R")
   ```

   The dashboard will open at `http://localhost:3838/`

---


---

## 📊 Dashboard Features

### 1. **Key Results** (Main Dashboard)
- View Quantities of Interest (QoI) with 95% confidence intervals
- Visual #1: Sales Performance comparison (Baseline vs. WMA forecast)
- Visual #2: Financial Impact with error bars
- Monthly trend analysis with project impact summary

### 2. **Problem Statement**
- Business context: Papemelroti's inventory challenges
- Root cause identification: Low turnover (2.62) → 139-day holding period
- Research question framing

### 3. **Methodology**
- Weighted Moving Average (WMA) forecasting explanation
- Inventory Turnover Optimization (ITO) framework
- DMAIC + Design Thinking integration
- Process flow comparison: Current state vs. Improved state

### 4. **Financial Impact**
- Cost comparison: Baseline vs. Target inventory levels
- Annual savings breakdown with 95% confidence intervals
- Scalability analysis for all 21 stores
- Capital release calculations

### 5. **Root Cause Analysis**
- 5 Whys systematic investigation
- Fault Tree Analysis (hierarchical problem decomposition)
- Visual identification of solution intervention points

### 6. **Interactive Simulator**
- Adjust reorder points, lead times, and safety stock
- Simulate different inventory policies
- View top items by holding cost
- Real-time impact on stockout risk

### 7. **Data Analysis**
- Performance metrics table with confidence intervals
- Raw data inspection (first 10 rows)
- Monthly sales forecast plot
- Inventory turnover progression tracking

### 8. **About & References**
- Project information and academic context
- Team members and sponsor details
- Key references and technical stack
- Complete list of deliverables and achievements

---

## 📁 Input Data Format

The dashboard accepts CSV files with the following required columns:

| Column | Description | Example |
|--------|-------------|---------|
| **Store** | Store location name | "Makati", "BGC", etc. |
| **Item Name** | Product identifier | "Notebook A5", "Bamboo Pen", etc. |
| **Month** | Month name | "Jan", "Feb", "Mar", etc. |
| **Number Stored in Inventory** | Current stock level | 150 |
| **Number Sold** | Units sold that month | 85 |
| **Cost (PHP)** | Total cost in Philippine Pesos | 2,550 |
| **Revenue (PHP)** | Total revenue in Philippine Pesos | 6,800 |

### Sample CSV Format

```csv
Store,Item Name,Month,Number Stored in Inventory,Number Sold,Cost (PHP),Revenue (PHP)
Makati,Notebook A5,Jan,120,65,1950,4550
Makati,Notebook A5,Feb,145,72,2160,5040
BGC,Bamboo Pen,Jan,200,95,2850,7125
```

---

## 🔧 Technical Architecture

### Tech Stack

- **Language:** R 4.0+
- **Framework:** Shiny (Interactive web application)
- **UI:** shinydashboard (Dashboard components)
- **Data Processing:** dplyr, tidyr, zoo (rolling windows)
- **Visualization:** ggplot2 (statistical graphics)
- **Diagramming:** DiagrammeR (DMAIC, Fault Tree, Process Flow)
- **Interactivity:** shinyalert (Modal alerts), DT (Data tables)

### Key Functions

#### `compute_metrics(df)`
Calculates core performance indicators:
- Sales volume, gross profit margin, inventory turnover
- Average inventory, holding period
- Returns cleaned data and all metrics

#### `compute_wma(df, weights = c(0.6, 0.3, 0.1))`
Implements Weighted Moving Average forecasting:
- Default weights: 60% (most recent), 30% (month-1), 10% (month-2)
- Computes WMA series and generates forecasts
- Returns actual vs. forecast comparison

#### `compute_confidence_intervals(df)`
Bootstrap validation (n=10,000):
- 95% CI for inventory turnover rate
- 95% CI for sales volume
- 95% CI for financial savings
- Statistical rigor for executive reporting

#### `clean_numeric(df, cols)`
Data cleaning utility:
- Converts specified columns to numeric
- Filters for finite values only
- Removes invalid rows

---

## 📈 Methodology

### Weighted Moving Average (WMA)

The forecasting model uses a responsive 3-month weighted average:

```
WMA_t = (0.6 × D_{t-1}) + (0.3 × D_{t-2}) + (0.1 × D_{t-3})
```

Where **D** = monthly demand (units sold)

**Why WMA?** More responsive to recent demand changes than simple moving average, ideal for retail inventory management.

### Inventory Turnover Optimization (ITO)

Links WMA forecasts to efficiency targets:

```
Target Avg Inventory = WMA Sales / 4.00
Reorder Point = Target - Current Inventory
```

**Target IT = 4.00** reduces holding period from 139 to 91 days (34.5% improvement).

### Six Sigma DMAIC Integration

| Phase | Our Application |
|-------|-----------------|
| **Define** | IT Rate = 2.62, Holding Period = 139 days |
| **Measure** | 6 months data: 150 observations, 5 stores |
| **Analyze** | 5 Whys + Fault Tree → Root cause: No forecasting |
| **Improve** | WMA + ITO implementation with interactive dashboard |
| **Control** | 95% CI validation, bootstrap (n=10,000) simulations |

---

## 📊 Key Findings

### Quantities of Interest (QoI)

1. **Inventory Turnover Rate:** 2.62 → 4.00 (+52.7%)
2. **Monthly Sales Volume:** 653.8 → 686.5 units (+5%)
3. **Gross Profit Margin:** 66.67% (maintained)
4. **Holding Period Reduction:** 139 → 91 days (-34.5%)
5. **Annual Savings:** ₱6,030 (5-store subset) | **95% CI: [₱4,258, ₱7,802]**
6. **Capital Released:** ₱30,149 for growth investments

### Financial Impact

**5-Store Subset:**
- Baseline Annual Holding Cost: ₱17,478
- Target Annual Holding Cost: ₱11,448
- **Annual Savings: ₱6,030** (Statistical validation with 95% confidence)

**Scaled to 21 Stores:**
- **Projected Annual Savings: ₱25,326**
- **Total Capital Released: ₱126,626**

---

## 🎓 Project Context

### Academic Information

- **Course:** SYSEN 5300 - Six Sigma, Cornell University, Fall 2025
- **Team:** Bradley Matican, Chris Lasa, Deepro Bandyopadhyay, Sreekar Mukkamala
- **Sponsor:** Papemelroti (Philippine retail SME, 40+ years, 21 stores)

### Data Source

- **Period:** 6 months (January - June 2024)
- **Coverage:** 5 stores, 5 product categories
- **Observations:** 150 records
- **Source:** Papemelroti POS system

### Key References

1. Silver, E. A., Pyke, D. F., & Thomas, D. J. (2017). *Inventory and Production Management in Supply Chains.* CRC Press.
2. Chopra, S., & Meindl, P. (2018). *Supply Chain Management: Strategy, Planning, and Operation.* Pearson Education.
3. Company operational data provided by Papemelroti Operations Team (2024-2025)

---

## 🤖 AI Assistant Feature

The dashboard includes an intelligent chatbot that answers common questions:

- "What's the current IT rate?" → Returns turnover status vs. target
- "What's the WMA forecast?" → Displays projected demand
- "What are the savings?" → Shows financial impact with CI
- "What's the holding period?" → Returns days and reduction metrics
- "What confidence level?" → Explains bootstrap validation

---

## 📋 Project Deliverables

✅ Project Charter & Problem Statement  
✅ SIPOC Diagram & Process Map  
✅ Voice of Customer (VOC) Tree  
✅ 5 Whys & Fault Tree Analysis  
✅ WMA Forecasting Model  
✅ ITO Framework Implementation  
✅ Bootstrap Statistical Validation  
✅ Interactive Shiny Dashboard (This App)  
✅ Financial Impact Analysis  
✅ Final Presentation & Report  


---

## ⚙️ Configuration & Customization

### Adjust WMA Weights

Modify the `compute_wma()` function call in the server to change forecasting sensitivity:

```r
# Default: 60% recent, 30% previous, 10% two months back
wma <- compute_wma(df, weights = c(0.6, 0.3, 0.1))

# More responsive (80% recent):
wma <- compute_wma(df, weights = c(0.8, 0.15, 0.05))
```

### Change Target IT Rate

Update the target inventory turnover throughout the app:

```r
# Search for "4.00" and replace with your target (e.g., "3.50")
# Affects: QoI calculations, visuals, simulator results
```

### Adjust Bootstrap Simulations

For faster processing or higher precision:

```r
# In compute_confidence_intervals(), modify n_bootstrap
n_bootstrap <- 5000    # Faster, less precise
n_bootstrap <- 25000   # Slower, more precise
```

---

## 🐛 Troubleshooting

### Common Issues

**Problem:** "Error loading data: Column names don't match"
- **Solution:** Verify CSV has exactly these columns (case-sensitive):
  - Store, Item Name, Month, Number Stored in Inventory, Number Sold, Cost (PHP), Revenue (PHP)

**Problem:** "No valid turnover data available"
- **Solution:** Check that "Number Stored in Inventory" > 0 for all rows

**Problem:** Dashboard loads but charts don't appear
- **Solution:** Clear browser cache or restart R session. Ensure all packages are installed.

**Problem:** AI Assistant returns generic response
- **Solution:** Try more specific questions like "current IT rate" or "WMA forecast"

See [TROUBLESHOOTING.md](docs/TROUBLESHOOTING.md) for more solutions.

---

## 📞 Support & Contact

For questions or issues:
1. Check the [troubleshooting guide](docs/TROUBLESHOOTING.md)
2. Review the [methodology documentation](docs/METHODOLOGY.md)
3. Examine the [data dictionary](docs/DATA_DICTIONARY.md)
4. Open an issue on GitHub with:
   - Specific error message
   - Sample data (anonymized if necessary)
   - Steps to reproduce

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 🎓 Academic Integrity

This project was completed as part of SYSEN 5300 (Six Sigma) at Cornell University. All work is original and follows academic integrity guidelines. If you use this project in your own work, please cite appropriately.

---

## ✨ Key Achievements

- **52.7%** reduction in process variation (inventory turnover)
- **34.5%** reduction in average holding period
- **₱6,030** annual savings (5 stores) with 95% statistical confidence
- **5%** projected sales growth through improved forecasting
- **Zero** capital investment required (process improvement only)
- **95%** confidence validated through bootstrap simulation (n=10,000)

---

**Made with ❤️ by the Cornell Six Sigma Black Belt Team**

*For Papemelroti | Fall 2025*
