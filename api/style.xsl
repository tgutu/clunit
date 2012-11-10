<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="html" doctype-public="-//W3C//DTD XHTML 1.1//EN"  doctype-system="http://www.w3.org/TR/xhtml11/DTD/xhtml1.1.dtd" />
    <xsl:template match="/asdf">
        <html xmlns="http://www.w3.org/1999/xhtml">
            <head>
                <title>
                    <xsl:value-of select="@name" /> API
                </title>
                <link href="style.css" rel="stylesheet" type="text/css" />
            </head>

            <body>                
                <div id="asdf-page-title">
                    <h1>API Documentation: <xsl:value-of select="@name"/></h1>
					<img src="lisplogo.png" style="left:2%;top:1.5%;position:absolute" width="90" length="90"/>
                </div>
				
				<h3 style="color:green;">Author(s)</h3>
				<div id="asdf-author">
					<xsl:value-of select="@author"/>
				</div>
             
				<h3 style="color:green;">Version</h3>
				<div id="asdf-version">
					<xsl:value-of select="@version"/>
				</div>
				
				<h3 style="color:green;">API Reference</h3>
                <div id="asdf-toc">
					<ol>
						<xsl:apply-templates select="symbols/symbol" mode="toc">
							<xsl:sort select="@name"/>
						</xsl:apply-templates>
					</ol>
                </div>
				
				<h3 style="color:green;">Description</h3>
				<div id="asdf-description">
					<xsl:copy-of select="description/node()"/>
                </div>
				
				<h3 style="color:green;">ASDF Naming Convention</h3>
                <div id="asdf-naming-convention">
					<p>
						Understanding the naming convention we use for ASDF systems is essential in navigating the various APIs with ease.<br/>
						When writing your own ASDF system please use the same notation to help simplify the organization process.
					</p>
					<p>
						An ASDF name can have one or more levels each separated by a dot i.e. '<b>.</b>' e.g. <b>xxx.yyy.zzz</b>. Each level is used to<br/> indicate
						the logical position of the system.	For example, the ASDF systems <b>xml.dom</b> and <b>xml.xpath</b> are logically<br/>
						considered subsystems of <b>xml</b> though they are defined separately. Breaking up one big complex system into<br/>
						subsystems makes it easier to develop it in small modular chunks.
					</p>
					<p>
						The first part of an ASDF name, i.e. the part before the first '<b>.</b>' is the package name of all the logical subsystems.<br/>
						For example the	ASDF systems <b>xml.dom</b> and <b>xml.xpath</b> are defined in the package <b>xml</b> and any exported symbol<br/>
						defined in the logical subsytems is accessed as <b>xml:symbol-name</b>.
					</p>
                </div>
				
				<h3 style="color:green;">Package API</h3>
                <div id="asdf-content">
                    <xsl:apply-templates select="symbols/symbol" mode="fulltext">
						<!-- <xsl:sort select="@name"/> -->
					</xsl:apply-templates>
                </div>
				<br/><br/>
            </body>
        </html>
    </xsl:template>
	
	<xsl:template match="symbol" mode="toc">
		<li>
			<a>
				<xsl:attribute name="href">#<xsl:value-of select="@name"/></xsl:attribute>
				<xsl:value-of select="@name"/>	
			</a>
		</li>		
	</xsl:template>
	
	<xsl:template match="symbol" mode="fulltext">
        <div class='symbol-syntax' >
		
			<xsl:attribute name="id"><xsl:value-of select="@name"/></xsl:attribute>
			<!-- Add an id attribute used by the Table of Contents to link to the function description. -->
			
			[<xsl:value-of select="@type"/>]<br/>
			<b><xsl:value-of select="@name"/></b> 
			<i>
				<xsl:if test="argument[@arg-type='whole']">
					&amp;whole <xsl:apply-templates select="argument[@arg-type='whole']"/>
				</xsl:if>
				
				<xsl:apply-templates select="argument[@arg-type='required']"/>
				
				<xsl:if test="argument[@arg-type='optional']">
					&amp;optional <xsl:apply-templates select="argument[@arg-type='optional']"/>
				</xsl:if>
				
				<xsl:if test="argument[@arg-type='key']">
					&amp;key <xsl:apply-templates select="argument[@arg-type='key']"/>
				</xsl:if>
				
				<xsl:if test="argument[@arg-type='rest']">
					&amp;rest <xsl:apply-templates select="argument[@arg-type='rest']"/>
				</xsl:if>
				
				<xsl:if test="argument[@arg-type='body']">
					&amp;body <xsl:apply-templates select="argument[@arg-type='body']"/>
				</xsl:if>
			</i>
			
			<!-- Test if the symbol has a return type (this only applies for functions and macros). If it does list the return type. -->
			<xsl:if test="return-type">
			=>
				<i>
					<xsl:for-each select="return-type">
						<xsl:value-of select="."/>&#160;
					</xsl:for-each>
				</i>
			</xsl:if>
			
			<!-- If the symbol takes any arguments (this only applies for functions and macros). If it does add two newlines before listing the arguments -->
			<xsl:if test="argument">
				<br/><br/>
			</xsl:if>
			<xsl:for-each select="argument">
				<div>[<xsl:value-of select="@value-type"/>] <i><xsl:value-of select="@name"/></i></div>
			</xsl:for-each>
		</div>

		<!-- <br/><b>Description</b><br/> -->
		<div class='function-description'>
			<xsl:copy-of select="description/node()"/>
		</div>
		<br/><br/>
    </xsl:template>
	
	<xsl:template match="argument">
       &#160;<xsl:value-of select="@name"/>
    </xsl:template>
</xsl:stylesheet>