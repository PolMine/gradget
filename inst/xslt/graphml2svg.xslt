<!--
-@author:  Christian Pich
-@contact:	Christian dot Pich at uni-konstanz dot de
-@name:		gnp.xsl
-@short:	Formats a spring.xsl graph
-@version:	1.0
-@modified:	
-@param: 	
-@desc: Transforms the output of spring.xsl into an SVG drawing
-of the graph.  Vertices are drawn as yellowish filled circles, and edges
-as black straight lines.
-->



<xsl:stylesheet version="2.0" 
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" 
>
  <xsl:output method="xml" indent="yes" encoding="utf-8"/>



<xsl:key name="node" match="node" use="@id"/>

<xsl:template match="desc|key|data"/>

<xsl:template match="/graphml/graph">
  <svg width="{data[@key='width']}" height="{data[@key='height']}">
    <xsl:apply-templates select="edge"/>
    <xsl:apply-templates select="node"/>
  </svg>
</xsl:template>

<xsl:template match="node">
  <circle r="3" style="stroke:black; fill:blue;"
    cx="{data[@key='v_x']}" cy="{data[@key='v_y']}"/>
   <a xlink:href="http://www.google.de">
   <text x="{data[@key='v_x']}" y="{data[@key='v_y']}" fill="red"  style="font-size:8px;font-family:sans-serif" >
    <xsl:value-of select="data[@key='v_name']"/>
   </text>
    </a>

</xsl:template>

<xsl:template match="edge">
  <line style="stroke:black; stroke-width:1px; fill:none;"
    x1="{key('node',@source)/data[@key='v_x']}" y1="{key('node',@source)/data[@key='v_y']}"
    x2="{key('node',@target)/data[@key='v_x']}" y2="{key('node',@target)/data[@key='v_y']}"/>
</xsl:template>

</xsl:stylesheet>
