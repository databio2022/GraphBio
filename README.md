# GraphBio
GraphBio: a shiny web app to easily perform popular visualization analysis for omics data

Website: http://www.graphbio1.com/ (chinese version) or http://www.graphbio1.com/en/ (english version)

Cite: Zhao T and Wang Z (2022), GraphBio: A shiny web app to easily perform popular visualization analysis for omics data. Front. Genet. 13:957317.doi: 10.3389/fgene.2022.957317

#### Use GraphBio in local system
On Linux:
1. `docker pull databio2022/graphbio:v1.0`
2. `docker run --rm -d -p 80:3838 -v /root/enlog/:/home/shiny/graphbio-log/ databio2022/graphbio:v1.0 /init`
3. Open Chrome browser and input `localhost`. You can use GraphBio locally.

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


