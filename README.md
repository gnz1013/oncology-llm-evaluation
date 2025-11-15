# oncology-llm-evaluation
Code and data repository for the manuscript:

“AI for Evidence-Based Treatment Recommendation in Oncology:
A Blinded Evaluation of LLMs and Agentic Workflows in Multiple Myeloma”

This repository contains the analysis scripts, de-identified data outputs, evaluation prompts, clinical scenarios, and result summaries used in the study. It is intended to support transparency and reproducibility of the analyses presented in the manuscript.

oncology-llm-evaluation/
│
├── code/                 # All R scripts for statistical analysis
│   ├── disagreement-ratio-4scale.R
│   ├── disagreement-ratio-binary.R
│   ├── domain-test-S.R
│   ├── ITR3-Fleiss-kappa-bootstrap.R
│   ├── T1-ICC.R
│   ├── T2-bootstrap-uncertainty-analysis.R
│   ├── T3-wilcoxon.R
│   └── TS1-meanRank.R
│
├── data/                 # Summary-level (non-identifiable) data used for plots and tables
│   ├── main-dat.xlsx
│   ├── domain_level_mean_ranks_accuracy.csv
│   └── ...
│
├── results/              # Output files from analyses
│   ├── ICC_summary_fixed.xlsx
│   ├── llm_bootstrap_results_R.xlsx
│   └── domain-analysis.csv
│
├── study-question/       # The 50 clinical scenarios used in the evaluation
│   └── 50 Clinical Scenarios.docx
│
├── prompt/               # Prompts used during model evaluation
│   └── system_prompt.txt
│
└── README.md
