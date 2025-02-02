# Property Cash Flow & Comparison Tool

The **Property Cash Flow & Comparison Tool** is a Shiny-based CRUD application built with R. It allows users to input property details, calculate expected monthly cash flows based on mortgage and operating cost inputs, and compare multiple properties side by side. All data is stored in a persistent SQLite database so that properties remain saved even after the app is reloaded.

## Features

- **User-Friendly Input Interface:**  
  Easily input property details including price, equity/down payment, square footage, mortgage information (interest rate, term, etc.), monthly operating expenses, and additional property details such as year built, number of rooms, bathrooms, and pool options.

- **Dynamic Calculations:**  
  Automatically calculates key financial metrics, including:
  - Mortgage payment (using a PMT-like function)
  - Total monthly cost (summing mortgage, HOA, taxes, landscaping, utilities, etc.)
  - Expected monthly cash flow (difference between expected rent and total costs)

- **CRUD Functionality:**  
  - **Create:** Add new property records.
  - **Read:** View properties and compare them side by side in a pivot table.
  - **Update:** Edit existing property details.
  - **Delete:** Remove properties from the database.

- **Persistent Data Storage:**  
  Uses SQLite to ensure that saved properties are retained even when the app is reloaded.

- **Modern UI with bslib:**  
  The app uses the **bslib** package for a modern, responsive UI that is fully customizable.

## Prerequisites

Before running the app, ensure you have the following installed:

- R (version 3.6 or later recommended)
- RStudio (optional, but recommended for development)
- The following R packages:
  - **shiny**
  - **DT**
  - **DBI**
  - **RSQLite**
  - **shinyjs**
  - **bslib**

You can install the required packages using:

```r
install.packages(c("shiny", "DT", "DBI", "RSQLite", "shinyjs", "bslib"))
```

## Installation

1. **Clone the Repository:**

   ```bash
   git clone https://github.com/yourusername/your-repository-name.git
   cd your-repository-name
   ```

2. **Open the Project in RStudio:**  
   Open the `.Rproj` file (if available) or simply open the `app.R` file.

## Usage

To run the application, execute the following command in R or RStudio:

```r
shiny::runApp("app.R")
```

Once the app is running, you can:

- Input your property details in the collapsible sidebar.
- Click **Save** to add a property to the database.
- Use the **Manage Properties** tab to edit or delete records.
- View calculated results and a side-by-side comparison of properties.

## Project Structure

- **app.R:** The main Shiny application script.
- **README.md:** This file.
- **properties.db:** The SQLite database file (created automatically on first run).

## Contributing

Contributions are welcome! If you find any issues or would like to suggest improvements:

1. Fork the repository.
2. Create a new branch for your changes.
3. Submit a pull request detailing your changes.

Please adhere to the repository's coding style and include tests for new features if possible.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Built with [Shiny](https://shiny.rstudio.com/), [DT](https://rstudio.github.io/DT/), and [bslib](https://rstudio.github.io/bslib/).
- Inspired by traditional Excel-based property cash flow analysis.

---