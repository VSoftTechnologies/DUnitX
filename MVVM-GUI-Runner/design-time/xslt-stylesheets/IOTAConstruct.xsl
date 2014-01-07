<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:msxsl="urn:schemas-microsoft-com:xslt"
  xmlns:t="https://github.com/SeanBDurkin/DUnitX/template">
<xsl:output method="text" indent="yes" encoding="utf-8" />
<xsl:strip-space elements="*" />
<xsl:preserve-space elements="t:stream"/>    

<xsl:param name="CompilerVersion" select="23.0" />
<xsl:param name="SourceType" select="'ProjectSource'" />
<xsl:param name="module-name" select="'GUIRunner'" />
<xsl:param name="path-translations"
	select="
		  '|=..
		   |MVVM-GUI-Runner\run-time\view=..\MVVM-GUI-Runner\run-time\view
		   |MVVM-GUI-Runner\run-time\backplane=..\MVVM-GUI-Runner\run-time\backplane
		   |MVVM-GUI-Runner\run-time\view-model=..\MVVM-GUI-Runner\run-time\view-model
		   |MVVM-GUI-Runner\run-time\view\plug-ins\trees=..\MVVM-GUI-Runner\run-time\view\plug-ins\trees
		   |MVVM-GUI-Runner\run-time\view\plug-ins\trees\DUnitX.VirtualStringTree=..\MVVM-GUI-Runner\run-time\view\plug-ins\trees\DUnitX.VirtualStringTree
		   '" />
<xsl:param name="plugin-units" select="'DUnitX.VirtualTrees DUnitX.VTAccessibility DUnitX.VTAccessibilityFactory'" />
<xsl:param name="plugin-unit-path" select="'..\..\Extenal-libraries\VirtualTreeView\Source'" />

<xsl:variable name="apos">'</xsl:variable>
    
<xsl:template match="@*|node()" />

<xsl:template match="/">
  <xsl:apply-templates select="t:IOTACreation/t:IOTACreator[
    t:CompilerVersion/@include='*' or
    t:CompilerVersion/@include=$CompilerVersion
     ]" />
</xsl:template>
      
<xsl:template match="t:IOTACreator">
  <xsl:apply-templates />
</xsl:template>
      
<xsl:template match="t:IOTAFile">
  <xsl:if test="t:*[@val='true' and $SourceType=local-name() and
                       ((local-name()='ProjectSource') or
                        (local-name()='ImplSource'   ) or
                        (local-name()='FormFile'     ))]">
    <xsl:apply-templates select="t:stream" />
  </xsl:if>  
</xsl:template>  
      
<xsl:template match="t:stream">
  <xsl:apply-templates />
</xsl:template>  

<xsl:template match="t:stream/text()">
  <xsl:value-of select="." />
</xsl:template>  
    
<xsl:template match="t:module-name">
  <xsl:value-of select="$module-name" />
</xsl:template>  
    
<xsl:template match="t:DUnitX-path">
  <xsl:variable name="key" select="concat('|',@plus,'=')" />
  <xsl:variable name="path2" select="substring-after( $path-translations, $key)" />
  <xsl:variable name="path3" select="normalize-space( substring-before( concat( $path2, '|'), '|'))" />
  <!-- Exclude trailing path delimter from $path3 to $path4 -->
  <xsl:variable name="path4" select="concat(substring($path3,1,string-length($path3)-1),
                                    translate( substring($path3,string-length($path3),1), '\', ''))"/>
  <xsl:value-of select="$path4" />
</xsl:template>  
   
<xsl:template match="t:plugin-units[@mode='dpr']">
  <xsl:call-template name="plugin-units-dpr">
    <xsl:with-param name="units" select="$plugin-units" />
  </xsl:call-template>
</xsl:template>  

<xsl:template name="plugin-units-dpr">
  <xsl:param name="units" />
   <xsl:variable name="first-unit" select="normalize-space( substring-before(concat( $units, ' '), ' '))" />
   <xsl:if test="$first-unit">
      <xsl:value-of select="concat('  ',$first-unit,' in ',$apos,$plugin-unit-path,'\',$first-unit,'.pas',$apos,',&#x0A;')" />
      <xsl:call-template name="plugin-units-dpr">
        <xsl:with-param name="units" select="substring-after( $units, concat($first-unit, ' '))" />
      </xsl:call-template>
   </xsl:if>
</xsl:template>  

<xsl:template match="t:plugin-units[@mode='pas']">
  <!-- xsl:value-of select="concat('  ,',$unit-name,'&#x0A;'" / -->
</xsl:template>  

<xsl:template match="t:plugin-view-units">
   TO BE DEVELOPED !
</xsl:template>

<xsl:template match="t:plugin-view-registration">
   TO BE DEVELOPED !
</xsl:template>


</xsl:stylesheet>