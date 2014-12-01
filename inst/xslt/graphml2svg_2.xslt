
<xsl:stylesheet version="2.0" 
xmlns="http://graphml.graphdrawing.org/xmlns"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:output method="xml" indent="yes" encoding="iso-8859-1"/>
	<xsl:param name="source">s</xsl:param><!-- global parameter -->
	<xsl:template match="data|desc|key"/>
	<xsl:template match="/graphml/graph">
		<graphml>
			<graph>
				<xsl:copy-of select="@*|*[name()!='node']"/>
				<key for="node" name="distance"/>
				<xsl:variable name="bfsnodes">
					<xsl:call-template name="bfs">
						<xsl:with-param name="V" select="node[@id!=$source]"/>
						<xsl:with-param name="W" select="node[@id=$source]"/>
						<xsl:with-param name="dist" select="number(0)"/>
					</xsl:call-template>
				</xsl:variable>
				<xsl:copy-of select="$bfsnodes/node"/>
				<xsl:for-each select="node[not(@id=$bfsnodes/node/@id)]">
					<xsl:copy>
						<xsl:copy-of select="*|@*"/>
						<data key="distance">-1</data><!-- not reachable -->
					</xsl:copy>
				</xsl:for-each>
			</graph>
		</graphml>
	</xsl:template>
	<xsl:template name="bfs">
		<xsl:param name="dist"/><!-- current distance to source -->
		<xsl:param name="V"/><!-- unvisited nodes -->
		<xsl:param name="W"/><!-- BFS front nodes -->
		<xsl:for-each select="$W">
			<xsl:copy>
				<xsl:copy-of select="*|@*"/>
				<data key="distance">
					<xsl:value-of select="$dist"/>
				</data>
			</xsl:copy>
		</xsl:for-each>
		<xsl:variable name="new" select="$V[@id=../edge[@source=$W/@id]/@target]"/>
		<xsl:if test="$new"><!-- newly visited nodes? -->
			<xsl:call-template name="bfs"><!-- start BFS from them -->
				<xsl:with-param name="V" select="$V[count(.|$new)!=count($new)]"/>
				<xsl:with-param name="W" select="$new"/>
				<xsl:with-param name="dist" select="$dist+1"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
