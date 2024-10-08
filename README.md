# Predictive Analysis of Customer Churn in Banks

## Overview

This project focuses on predicting customer churn in the banking sector using machine learning techniques. Customer churn is a critical issue for banks, where customers stop using a bank's services over a given period. By predicting churn, banks can take preemptive actions to retain valuable customers.

## Features

- **Data Exploration**: Comprehensive analysis of the dataset to identify trends, correlations, and patterns related to customer churn.
- **Data Preprocessing**: Handling missing values, encoding categorical variables, and normalizing data for better model performance.
- **Modeling**: Implementation of various machine learning models to predict customer churn, including Logistic Regression, Random Forest, and Support Vector Machines.
- **Evaluation**: Assessment of model performance using metrics such as accuracy, precision, recall, F1-score, and AUC-ROC curves.
- **Visualization**: Detailed plots and charts to visualize the data and model results.

## Installation

1. Clone the repository:
    ```bash
    git clone https://github.com/Gaboelc/predictive-analysis-of-customer-churn-in-banks.git
    cd predictive-analysis-of-customer-churn-in-banks
    ```

2. Create and activate a virtual environment:
    ```bash
    python3 -m venv venv
    source venv/bin/activate
    ```

3. Install the required packages:
    ```bash
    pip install -r requirements.txt
    ```

## Usage

1. **Data Exploration**: Run the `EDA.ipynb` notebook to explore the dataset and understand the features influencing customer churn.

2. **Model Training**: Use `model_training.ipynb` to train different machine learning models on the processed data.

3. **Prediction**: After training, use the models to predict customer churn on new data.

4. **Evaluation**: Evaluate the models using the `evaluation.ipynb` notebook to determine the best performing model.

## Datasets

The dataset used in this project is included in the `data/` directory. It consists of various customer attributes such as demographics, account information, and transaction history.

## Results

- The Random Forest model showed the highest accuracy with significant precision and recall, making it the best choice for this predictive task.
- Detailed evaluation results and plots are available in the notebooks for further analysis.

## Contributing

Contributions are welcome! Please follow these steps:
1. Fork the repository.
2. Create a new branch (`git checkout -b feature-branch`).
3. Commit your changes (`git commit -am 'Add new feature'`).
4. Push to the branch (`git push origin feature-branch`).
5. Create a new Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The project is inspired by the growing need to retain customers in the highly competitive banking industry.
- Thanks to the open-source community for the tools and libraries used in this project.

## Contact

For any questions or inquiries, please reach out via the repository's issue tracker.
