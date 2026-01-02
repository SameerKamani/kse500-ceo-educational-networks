# Pathways to Power: Educational Networks in Pakistan's Corporate Elite

## Project Overview
This repository hosts the source code, dataset, and analysis for the research paper **"Pathways to Power: A Network Analysis of Educational Pipelines in the KSE-500"** by Rana Mohammad Sarib Khan and Sameer Kamani (December 5, 2025). 

Using social network analysis (SNA), we examine the educational backgrounds of 298 CEOs from Pakistan's top 500 listed companies (KSE-500). The study uncovers a scale-free, small-world network dominated by "super-hubs" like the Institute of Business Administration (IBA) and Punjab University. It reveals regional fragmentation (e.g., Karachi vs. Lahore) bridged by international institutions, highlighting how shared alma maters reinforce elite reproduction and form an "Old Boys Club" among corporate leaders.

Key insights include:
- **Bipartite Network**: Extremely sparse (density 0.004) with power-law distribution, confirming "rich-get-richer" dynamics where top institutions attract more aspirants.
- **Institution Projection**: Divided into 4 main clusters:
  - **Tech-Business Nexus (Karachi)**: Dense ties between IBA and engineering schools like NED.
  - **Lahore-International Axis**: Prestige links from LUMS/UET Lahore to Oxbridge.
  - **National Legacy Cluster**: Public giants like Punjab University and professional bodies (ICAP).
  - **UK Cluster**: Finance-focused around LSE and ICAEW.
- **CEO Projection**: Highly cohesive with 80% in a giant component, high clustering (0.88), and short paths (3.17), indicating rapid information flow in elite circles.
- Network validated against models like Barabási–Albert (scale-free) and Watts–Strogatz (small-world).

This project demonstrates how education acts as a gateway—and barrier—to corporate power in Pakistan, mirroring global "small club" dynamics.

## Data Sources
- Curated from PSX, Stock-Analysis.com, LinkedIn, Market Screener, and annual reports.
- Includes 298 CEOs and 171 institutions (filtered for completeness).
- **Note**: Static 2025 snapshot; may have biases toward publicly visible profiles. Dataset available in `data/` (anonymized where needed).

## Key Visualizations
The project's visualizations are central to understanding the network structures, clusters, and distributions.

- **Figure 1: Top 10 Institutions by Degree**  
  ![Figure 3](figures/figure3_top10_bar.png)  
  *Bar graph showing IBA leading with 34 CEOs. (Placeholder: Add your extracted image here.)*

- **Figure 2: Giant Component of the Institution-Institution Projection**  
  ![Figure 4](figures/figure4_giant_component.png)  
  *Visualizes the core academic collaboration network based on shared alumni. (Placeholder: Add your extracted image here.)*

- **Figure 3: Communities within the Institution-Institution Projection**  
  ![Figure 5](figures/figure5_communities.png)  
  *Color-coded clusters in the giant component. (Placeholder: Add your extracted image here.)*

- **Figure 4: Giant Component of the CEO-CEO Projection**  
  ![Figure 10](figures/figure10_ceo_component.png)  
  *Illustrates the "Old Boys Club" with shared alma maters. (Placeholder: Add your extracted image here.)*



Additionally, key tables (e.g., centrality measures) can be rendered as Markdown in the full report or notebooks for deeper dives.

## Limitations & Future Work
- **Limitations**: Incomplete data (e.g., missing undergrad details) introduces bias; static snapshot misses generational trends.
- **Future Directions**: Enhance dataset completeness, differentiate by degree levels (Bachelor's/Master's/PhD), and deepen CEO community analysis.

## References
See the full report  `Research_Report.pdf` for detailed citations. Key works include Hambrick's Upper Echelons Theory and Semenova's SNA on German executives.

## License
This project is licensed under the MIT License—feel free to use, modify, and contribute!

---

If you have questions, spot issues, or want to collaborate, open an issue or submit a pull request. Let's uncover more insights into corporate networks! 🚀
