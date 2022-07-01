//bubble js d3 version 3
    var diameter = 800, //max size of the bubbles
      color = d3.scale.category20(); //color category

    var bubble = d3.layout.pack()
      .sort(null)
      .size([diameter, diameter])
      .padding(1.5);

    //var svg = d3.select("section")
    //  .append("svg")
    //  .attr("width", diameter)
    //  .attr("height", diameter)
    //  .attr("class", "bubble");

      //d3.csv("go_term_bubble.csv", function(error, data) {
      
      //var data = [{"Item":"Daily Phone Pick-ups on Average: 183 Counts ","Amount":"182"},{"Item":"Daily Screen Time: 401 minutes","Amount":"401.4"},{"Item":"Daily Walking & Running Distance: 0.188 miles","Amount":"188.36"},{"Item":"Daily Steps: 44 Counts","Amount":"448"},{"Item":"Daily Flights Climbed: 39 Counts","Amount":"393"}];
      
      //console.log(data[0].value);

      //convert numerical values from strings to numbers
      data = data.map(function(d) {
        d.value = +d["value"];
        return d;
      });

      //bubbles needs very specific format, convert data to this.
      var nodes = bubble.nodes({
        children: data
      }).filter(function(d) {
        return !d.children;
      });

      //setup the chart
      var bubbles = svg
        .attr("id","mysvg")
        .attr("xmlns","http://www.w3.org/2000/svg")
        .append("g")
        .attr("transform", "translate(0,0)")
        .selectAll(".bubble")
        .data(nodes)
        .enter();

      //create the bubbles
      bubbles.append("circle")
        .attr("r", function(d) {
          return d.r;
        })
        .attr("cx", function(d) {
          return d.x;
        })
        .attr("cy", function(d) {
          return d.y;
        })
        .style("fill", function(d) {
          return color(d.value);
        });

      //format the text for each bubble
      bubbles.append("text")
        .attr("x", function(d) {
          return d.x;
        })
        .attr("y", function(d) {
          return d.y;
        })
        .attr("text-anchor", "middle")
        .text(function(d) {
          return d["show"];
        })
        .style({
          "fill": "black",
          "font-family": "Verdana, san-serif",
          "font-size": "8px"
        })
      .each(wrap);
    //})
    //var svg = document.getElementById("svg");

    function wrap(d) {
        var text = d3.select(this),
          width = d.r * 2,
          x = d.x,
          y = d.y-5,
          words = text.text().split(/\s+/).reverse(),
          word,
          line = [],
          lineNumber = 0,
          lineHeight = 1.1,
          tspan = text.text(null).append("tspan").attr("x", x).attr("y", y);
        while (word = words.pop()) {
          line.push(word);
          tspan.text(line.join(" "));
          if (tspan.node().getComputedTextLength() > width) {
            line.pop();
            tspan.text(line.join(" "));
            line = [word];
            tspan = text.append("tspan").attr("x", x).attr("y", y).attr("dy", ++lineNumber * lineHeight + "em").text(word);
          }
        }
    }
