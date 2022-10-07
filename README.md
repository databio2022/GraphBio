# GraphBio
GraphBio: a shiny web app to easily perform popular visualization analysis for omics data

Website: http://www.graphbio1.com/ (chinese version) or http://www.graphbio1.com/en/ (english version)

Cite: Zhao T and Wang Z (2022), GraphBio: A shiny web app to easily perform popular visualization analysis for omics data. Front. Genet. 13:957317.doi: 10.3389/fgene.2022.957317

#### Use GraphBio in local system
On Linux:
1. `docker pull databio2022/graphbio:v1.0`
2. `docker run --rm -d -p 80:3838 -v /root/enlog/:/home/shiny/graphbio-log/ databio2022/graphbio:v1.0 /init`
3. Open Chrome browser and input `localhost`. You can use GraphBio locally.

