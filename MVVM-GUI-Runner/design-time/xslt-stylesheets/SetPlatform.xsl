<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:msb="http://schemas.microsoft.com/developer/msbuild/2003"
  exclude-result-prefixes="xsl msb">
<xsl:output indent="yes" omit-xml-declaration="yes" />

<xsl:param name="platform" select="'Win64'" />

<xsl:template match="@*|node()">
  <xsl:copy>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="msb:PropertyGroup/msb:Platform/text()">
  <xsl:value-of select="$platform" />
</xsl:template>

</xsl:stylesheet>
