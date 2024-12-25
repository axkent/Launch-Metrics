# Closeread Project: Launch Metrics in a Baseball Series

![READMEdemo](https://github.com/user-attachments/assets/137e5c2a-b5ff-41bc-80ad-827cbb7a6431)

## Overview
This repository contains materials for my [Closeread](https://closeread.dev/) project, an analysis of the 2024 Dodgers-Padres playoff matchup. The project includes:

- A Closeread article that explores the series' launch angle metrics, with a particular focus on home runs. 
- A Shiny app providing interactive visualizations and analysis of launch angle data. Most of the visualizations in the Closeread article were retrieved from the app. 
- Supporting data, a data retrieval script, and visualizations to build the article.

You can view the published Closeread article here:
[Closeread Article](https://axkent.quarto.pub/closeread-vectors-of-victory/)

The Shiny app is hosted here:
[Shiny App](https://axelkent.shinyapps.io/LaunchMetricsApp/)

---
### How to access the Closeread article locally
1. Install the Closeread extension from [https://closeread.dev/](https://closeread.dev/).
2. Run `index.qmd` in RStudio
---
### How to Access the Shiny App locally

1. Navigate to the `shiny` folder
2. Open `app.R` in RStudio and click **Run App**.

---

### Data and Visualizations
- **Data**: The data used in the analysis is stored in the `shiny/` folder. You can regenerate the dataset using the `retrieve_data.R` script.
- **Visualizations**: The visualizations, including those created by the Shiny app and others used in the Closeread article, are in the `visualizations/` folder.

---

### Acknowledgments
This is my project for submission to the [Closeread Prize](https://posit.co/blog/Closeread-prize-announcement/). Big thank you to the Posit community for their help and putting this event together.

I would like to acknowledge [Jim Albert](https://gist.github.com/bayesball), for his visualizations created on his blog post titled ["Zack Wheeler’s Pitching in the 2023 NLCS"](https://baseballwithr.wordpress.com/2023/10/23/zack-wheelers-pitching-in-the-2023-nlcs/) encouraged me to do a deep dive on the 2024 Dodgers-Padres playoff series. Albert's code used to generate that blog post's visualizations is found [here](https://gist.github.com/bayesball/a1f8ddb4593e7b31b83022e511f5e560).

I would also like to acknowledge [Robert Frey](https://github.com/robert-frey), for his YouTube tutorial titled ["Combine Video with a Savant Dataset!"](https://www.youtube.com/watch?v=a_fIJxuaQL8) and code demonstrated how to retrieve videos for each observation. Frey's code is available [here](https://github.com/robert-frey/YouTube/blob/master/Combine%20Video%20with%20a%20Savant%20Dataset!/savant_videos.R).


---

### License
This project is licensed under the MIT License.

