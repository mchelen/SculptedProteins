<%@ Page Language="VB" Debug="true" %>
<%@ Import Namespace="System.Drawing" %>
<%@ Import Namespace="System.Drawing.Text" %>
<%@ Import Namespace="System.Drawing.Imaging" %>
<%@ Import Namespace="System.Net" %>
<%@ Import Namespace="System.IO" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<!-- 
MIT license
Copyright (c) 2009 Andrew SID Lang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
-->   
    <script language="VB" runat="server">

Sub Page_Load(sender As Object, e As EventArgs)
        
            Response.Write("<html>")
            Response.Write("<head>")
            Response.Write("<meta name='description' content='ASID Media Design is an official Second Life Solution Provider for educational, science-related and nonprofit organizations.' />")
            Response.Write("<meta name='keywords' content='second life, second life solution provider, second life consultant, second life developer, second life development, second life design, colleges in second life, universities in second life, second life nonprofit, second life non-profit, second life education, science in second life, second life brands, second life platform, buying land in second life, establishing a presence in Second Life, second life island developer, second life sim developer, ASID Media, Hiro Sheridan, second life companies' />")
            Response.Write("<title>ASID Media Design | Sculpted Proteins in Second Life</title>")
            Response.Write("<link href='../slusage.css' rel='stylesheet' type='text/css'>")
            Response.Write("</head>")
            Response.Write("<body>")
            Response.Write("<table width='900' align='center' border='0'>")
            Response.Write("<tr><td valign='top'><a href='../index.asp'><img src='../images/Header.jpg' alt='ASID Media Design | Second Life Solution Provider' border='0' /></a></td></tr></table>")

            
            
            Response.Write("<img src='s.png'><span>creating sculpted map texture</span>")
            Response.Write("<br />")
            Response.Flush()
        'Create the HttpWebRequest object to pdb data file
        Dim req As HttpWebRequest = WebRequest.Create(Request("pdb"))

        Try
            'Get the data as an HttpWebResponse object
                Response.Write("<img src='s.png'>downloading file<br />")
            Response.Flush()
            Dim resp As HttpWebResponse = req.GetResponse()

            'Convert the data into a string (assumes that you are requesting text)
            Dim sr As New StreamReader(resp.GetResponseStream())
            Dim results As String = sr.ReadToEnd()
            sr.Close()
                Response.Write("<img src='s.png'>download complete, parsing file<br />")
            Response.Flush()
            Dim strparse As String
            Dim n As Integer = 0
            Dim xarr(50000), yarr(50000), zarr(50000) As Double
                Dim xtot, ytot, ztot, xave, yave, zave As Double
                Dim residue(50000) As String
                
            xtot = 0
            ytot = 0
            ztot = 0
            results = Right(results, Len(results) - InStr(results, "ATOM      1"))
            Do While (InStr(results, "ATOM ") > 0)
                n = n + 1
                    strparse = Left(results, 53)
                    residue(n) = Mid(strparse, 17, 3)
                xarr(n) = Mid(strparse, 30, 8)
                xtot = xtot + xarr(n)
                yarr(n) = Mid(strparse, 38, 8)
                ytot = ytot + yarr(n)
                zarr(n) = Mid(strparse, 46, 8)
                ztot = ztot + zarr(n)
                    'Response.Write(Mid(strparse, 30, 8) & "|" & Mid(strparse, 38, 8) & "|" & Mid(strparse, 46, 8) & "<br />")
                    'Response.Flush()
                results = Right(results, Len(results) - InStr(results, "ATOM "))
            Loop
                Response.Write("<img src='s.png'>parsing complete, centering coordinates<br />")
            Response.Flush()
            xave = xtot / n
            yave = ytot / n
            zave = ztot / n
            Dim i, j, k As Integer
            'center coordinates
            For i = 1 To n
                xarr(i) = xarr(i) - xave
                yarr(i) = yarr(i) - yave
                zarr(i) = zarr(i) - zave
                'Response.Write(xarr(i) & "|" & yarr(i) & "|" & zarr(i) & "<br />")
                'Response.Flush()
            Next
                Response.Write("<img src='s.png'>coordinates centered, calculating surface - please be patient<br />")
            Response.Flush()
            'calculate surface points
                Dim surfacex(16385), surfacey(16385), surfacez(16385), surfaceresidue(16385)
            
            
            Dim m As Integer = 128 '128 for full sculptie map
            
            Dim theta, phi, unitx, unity, unitz, sculptmax, pointx, pointy, pointz, rdistance, maxdistance As Double
            sculptmax = 0
            maxdistance = 0
      
            For k = 1 To n
                rdistance = Math.Sqrt(xarr(k) ^ 2 + yarr(k) ^ 2 + zarr(k) ^ 2)
                If rdistance > maxdistance Then
                    maxdistance = rdistance
                End If
            Next
                Response.Write("<img src='s.png'>maxdistance= " & maxdistance & "<br />")
            Response.Flush()
                Response.Write("<img src='s.png'>|................................................................................................................................|<br /><img src='s.png'>|")
            
                Dim disttopoint, disttoline, closestdist As Double
                Dim residueofpoint As Integer
            Dim l As Integer
                For j = 0 To m - 1
                    Response.Write(".")
                    Response.Flush()
                    For i = 0 To m - 1
                        theta = i * 2 * Math.PI / m '0 to 2pi
                        phi = -(Math.PI / 2) + Math.PI * j / m '-pi/2 to pi/2
                        unitx = Math.Cos(phi) * Math.Cos(theta)
                        unity = Math.Cos(phi) * Math.Sin(theta)
                        unitz = Math.Sin(phi)
                    
                        pointx = maxdistance * unitx
                        pointy = maxdistance * unity
                        pointz = maxdistance * unitz
                        rdistance = maxdistance
                    
                        For l = 1 To 2
                        
                            closestdist = maxdistance
                       
                            For k = 1 To n
                                disttoline = Math.Sqrt((unity * zarr(k) - unitz * yarr(k)) ^ 2 + (unitx * zarr(k) - unitz * xarr(k)) ^ 2 + (unity * xarr(k) - unitx * yarr(k)) ^ 2)
                                disttopoint = Math.Sqrt((pointx - xarr(k)) ^ 2 + (pointy - yarr(k)) ^ 2 + (pointz - zarr(k)) ^ 2)
                                If disttoline < 5 Then 'default=4, increase if too much grey
                                
                            
                                    If disttopoint < closestdist Then
                                        closestdist = disttopoint
                                        residueofpoint = k
                                    End If
                                
                                End If
                            
                            Next
                        
                            rdistance = rdistance - closestdist
                            pointx = rdistance * unitx
                            pointy = rdistance * unity
                            pointz = rdistance * unitz
                                       
                        Next
                        'Response.Write(closestdist & "<br />")
                        'Response.Flush()
                    
                        surfacex(j * m + i) = pointx
                        surfacey(j * m + i) = pointy
                        surfacez(j * m + i) = pointz
                        surfaceresidue(j * m + i) = residue(residueofpoint)
                   
                        sculptmax = Math.Max(sculptmax, Math.Abs(pointx))
                        sculptmax = Math.Max(sculptmax, Math.Abs(pointy))
                        sculptmax = Math.Max(sculptmax, Math.Abs(pointz))
                    Next
                Next
                Response.Write("|<br /><img src='s.png'>surface calculated, preparing sculpt map<br />")
                Response.Write("<img src='s.png'>sculptmax= " & sculptmax & "<br />")
            Response.Flush()
            Dim colr, colg, colb As Integer
            
            
            'draw scupltie map
            Dim oBitmap As Bitmap = New Bitmap(128, 128)
            Dim oGraphic As Graphics = Graphics.FromImage(oBitmap)
            Dim oColor As System.Drawing.Color
                      
            Dim oBrush As New SolidBrush(oColor)
            Dim oBrushWrite As New SolidBrush(Color.Gray)

            oGraphic.FillRectangle(oBrush, 0, 0, 128, 128)
            

          
            
            For i = 0 To m - 1
                For j = 0 To m - 1
                    colr = 128 + Int(127 * surfacex(j * m + i) / sculptmax)
                    colg = 128 + Int(127 * surfacey(j * m + i) / sculptmax)
                    colb = 128 + Int(127 * surfacez(j * m + i) / sculptmax)
                    oColor = Color.FromArgb(colr, colg, colb)
                    oBitmap.SetPixel(i, (127 - j), oColor)
                Next
            Next
              
            oBitmap.Save(Server.MapPath("gen_img.tiff"), ImageFormat.Tiff)
                Response.Write("<img src='s.png'><br /><img src='s.png'><b>Sculpt Map</b><br/><img src='s.png'><img src=""gen_img.tiff"">")
            
                'Now do residue color: http://jmol.sourceforge.net/jscolors/
                oGraphic.FillRectangle(oBrush, 0, 0, 128, 128)
               
            
                For i = 0 To m - 1
                    For j = 0 To m - 1
                        Select Case surfaceresidue(j * m + i)
                        
                            Case "ALA"
                                oColor = Color.FromArgb(200, 200, 200)
                            Case "ARG"
                                oColor = Color.FromArgb(20, 90, 255)
                            Case "ASN"
                                oColor = Color.FromArgb(0, 220, 220)
                            Case "ASP"
                                oColor = Color.FromArgb(230, 10, 10)
                            Case "CYS"
                                oColor = Color.FromArgb(230, 230, 0)
                            Case "GLN"
                                oColor = Color.FromArgb(0, 220, 220)
                            Case "GLU"
                                oColor = Color.FromArgb(230, 10, 10)
                            Case "GLY"
                                oColor = Color.FromArgb(235, 235, 235)
                            Case "HIS"
                                oColor = Color.FromArgb(130, 130, 210)
                            Case "ILE"
                                oColor = Color.FromArgb(15, 130, 15)
                            Case "LEU"
                                oColor = Color.FromArgb(15, 130, 15)
                            Case "LYS"
                                oColor = Color.FromArgb(20, 90, 255)
                            Case "MET"
                                oColor = Color.FromArgb(230, 230, 0)
                            Case "PHE"
                                oColor = Color.FromArgb(50, 50, 170)
                            Case "PRO"
                                oColor = Color.FromArgb(220, 150, 130)
                            Case "SER"
                                oColor = Color.FromArgb(250, 150, 0)
                            Case "THR"
                                oColor = Color.FromArgb(250, 150, 0)
                            Case "TRP"
                                oColor = Color.FromArgb(180, 90, 180)
                            Case "TYR"
                                oColor = Color.FromArgb(50, 50, 170)
                            Case "VAL"
                                oColor = Color.FromArgb(15, 130, 15)
                            Case "ASX"
                                oColor = Color.FromArgb(255, 105, 180)
                            Case "GLX"
                                oColor = Color.FromArgb(255, 105, 180)
                            Case Else
                                oColor = Color.FromArgb(190, 160, 110)
                        End Select
                        
                        oBitmap.SetPixel(i, (127 - j), oColor)
                    Next
                Next
              
                oBitmap.Save(Server.MapPath("gen_img2.tiff"), ImageFormat.Tiff)
                Response.Write("<img src='s.png'><br /><img src='s.png'><b>Amino Acid Residue</b><br/><img src='s.png'><img src=""gen_img2.tiff"">")
           
                'Now do amino acid charge: http://en.wikipedia.org/wiki/Amino_acids#Table_of_standard_amino_acid_abbreviations_and_side_chain_properties
                oGraphic.FillRectangle(oBrush, 0, 0, 128, 128)
               
            
                For i = 0 To m - 1
                    For j = 0 To m - 1
                        Select Case surfaceresidue(j * m + i)
                        
                            Case "ALA"
                                oColor = Color.FromArgb(255, 255, 255) 'neutral
                            Case "ARG"
                                oColor = Color.FromArgb(0, 0, 255) 'positive
                            Case "ASN"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "ASP"
                                oColor = Color.FromArgb(255, 0, 0) 'negative
                            Case "CYS"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "GLN"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "GLU"
                                oColor = Color.FromArgb(255, 0, 0)
                            Case "GLY"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "HIS"
                                oColor = Color.FromArgb(0, 0, 255)
                            Case "ILE"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "LEU"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "LYS"
                                oColor = Color.FromArgb(0, 0, 255)
                            Case "MET"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "PHE"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "PRO"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "SER"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "THR"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "TRP"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "TYR"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "VAL"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "ASX"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "GLX"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case Else
                                oColor = Color.FromArgb(255, 255, 255)
                        End Select
                        
                        oBitmap.SetPixel(i, (127 - j), oColor)
                    Next
                Next
              
                oBitmap.Save(Server.MapPath("gen_img3.tiff"), ImageFormat.Tiff)
                Response.Write("<img src='s.png'><br /><img src='s.png'><b>Residue Charge</b><br/><img src='s.png'><img src=""gen_img3.tiff"">")
                
                'Now do amino acid polarity: 
                oGraphic.FillRectangle(oBrush, 0, 0, 128, 128)
               
            
                For i = 0 To m - 1
                    For j = 0 To m - 1
                        Select Case surfaceresidue(j * m + i)
                        
                            Case "ALA"
                                oColor = Color.FromArgb(255, 255, 255) 'non polar
                            Case "ARG"
                                oColor = Color.FromArgb(0, 255, 0) 'polar
                            Case "ASN"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "ASP"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "CYS"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "GLN"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "GLU"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "GLY"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "HIS"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "ILE"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "LEU"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "LYS"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "MET"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "PHE"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "PRO"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "SER"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "THR"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "TRP"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "TYR"
                                oColor = Color.FromArgb(0, 255, 0)
                            Case "VAL"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "ASX"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case "GLX"
                                oColor = Color.FromArgb(255, 255, 255)
                            Case Else
                                oColor = Color.FromArgb(255, 255, 255)
                        End Select
                        
                        oBitmap.SetPixel(i, (127 - j), oColor)
                    Next
                Next
              
                oBitmap.Save(Server.MapPath("gen_img4.tiff"), ImageFormat.Tiff)
                Response.Write("<img src='s.png'><br /><img src='s.png'><b>Polarity</b><br/><img src='s.png'><img src=""gen_img4.tiff"">")
                
        Catch wex As WebException
                Response.Write("<img src='s.png'><font color=red>SOMETHING WENT AWRY!<br />Status: " & wex.Status & "Message: " & wex.Message & "</font>")
        End Try


End Sub

</script> 
<table width="900" align="center">
<tr>
<td>
<span>how to upload your protein to Second Life</span><br />
1. Save the above image to your desktop (right-click on it and select 'save as'). <br />
2. Start Second Life.<br />
3. Go to File -> Upload and upload the image file you just saved.<br />
4. Rez a sphere on the ground and change the object type to sculpted.<br />
5. Drag your just uploaded texture onto the object's sculpt texture.<br />
6. The other images are optional surface textures to color your protein surface.<br />
</td>
</tr>
</table>
</body>
</html>
