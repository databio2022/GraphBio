# GraphBio---A modular and scalable R Shiny dashboard
GraphBio: a shiny web app to easily perform popular visualization analysis for omics data

Website: http://www.graphbio1.com/ (chinese version) or http://www.graphbio1.com/en/ (english version)

Cite: Zhao T and Wang Z (2022), GraphBio: A shiny web app to easily perform popular visualization analysis for omics data. Front. Genet. 13:957317.doi: 10.3389/fgene.2022.957317

# Deploying to your own web server with docker.
`docker pull databio2022/graphbio:v2.2.7-manual` <br>
`docker run --rm -d -p 80:3838 -v /root/log/:/home/shiny/graphbio-log/ databio2022/graphbio:v2.2.7-manual /init`

For english version, please use: <br>
`docker pull databio2022/graphbio:v2.2.5-en-manual` <br>
`docker run --rm -d -p 80:3838 -v /root/log/:/home/shiny/graphbio-log/ databio2022/graphbio:v2.2.5-en-manual /init`

#### Demo data for each visualization function (files are placed in the graphbio-english/www/ directory)
1. heatmap_test.csv and group_info.csv for heatmap.
2. volcano_example.csv and volcano_example1.csv for volcano plot.
3. cdc_example.csv and cdc-mutiple-group.csv for Cumulative Distribution Curves
4. chord_example.csv for chord plot
5. corr_net.csv for network plot
6. corr_scatter.csv for correlation scatter plot
7. go_bubble_example.csv for go dotplot style 2
8. go_term_bubble.csv for go dotplot style 1
9. ma_example.csv for MA plot
10. pie_example.csv for pie plot
11. roc_example.csv for ROC curve
12. sixiangxian_example.csv for Four Quadrant Diagrams
13. surv_example.csv for survival curves
14. text_cluster_example.csv for cluster analysis
15. venn_example.csv for venn plot


