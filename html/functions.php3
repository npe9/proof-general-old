<?php
//
// Dave's PHP3 Header
//
// Included in every page.  
// Prints DTD and defines some useful functions.
//
// David Aspinall, June 1999.
//
// $Id$
//
//


// DTD 

$dtd_strict = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
$dtd_loose = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";

print $dtd_loose;

// Validator address

$validator = "http://validator.dcs.ed.ac.uk/";
// $validator = "http://localhost/validator/";

/* Some handy constants */

$pg_email = "proofgen@dcs.ed.ac.uk";
$pg_list = "proofgeneral@dcs.ed.ac.uk";

$pg_title = "Proof General --- Organize your Proofs!";

function mlink($addr) {
  print "<a href=\"mailto:" . $addr . "\">" . $addr . "</a>";
}

function pg_email() {
  mlink("proofgen@dcs.ed.ac.uk");
}


/* Style sheet element for dt doesnt work in Netscape 4, so hack it here.  
   NB!  This violates HTML 4 DTD.
*/

function dt($string) {
  print "<dt><div style=\"font-style:italic; font-weight: bold\">";
  print $string;
  print "</div></dt>";
/* Good version, okay in IE: 
  print "<dt>" . $string . "</dt>";
*/
}

/* Automatic footnotes? */
/* FIXME: for now, just inline them. */

function footnote ($text) {
   print "<p><small><i>[" . $text . "]</i></small></p>";
}

/* A hyper-link with optional mouse over text.
   Could be made more sophisticated to do roll-overs, 
   or whatever.
*/

function hlink ($url,$text,$mouseover="") {
  print "<a href=\"" . $url . "\"";
  if ($mouseover != "") {
	print " onMouseOver=\"window.status='" . $mouseover . "'; return true;\"";
  };
  print ">" . $text . "</a>";
}

/* Determining download sizes (print broken message if file not found) */

function download_size($filename) {
   if (file_exists($filename)) {
	$size = filesize($filename);
        $sizek = (int) ($size/1024);
   	print " (";
   	if ($sizek == 0) {
	     print $size . " bytes)";
   	} else {
	     print $sizek . "k)";
   	}
   } else {
	print " (link broken)";
   }
}

function download_link($filename,$linkname = "") {
   print "<a href=\"" . $filename . "\">";
   if ($linkname == "") { 
	print $filename;
   } else {
	print $linkname;
   };
   print "</a>";
   print download_size($filename);
}

function date_modified($filename) {
   $time = filemtime($filename);
   if ($time) {
	print "Last modified " . strftime("%d %B %Y",$time) . ".\n";
   }
}

/* Nav bar separator */

$separator  =  ' <img src="images/bullethole.gif" alt="." align=top> ';

/* A link to one of the main pages (must appear in navbar menu) */

function link_root($page,$text) {
    print  "<a  href=\"index.phtml?page=" . $page . "\">";
    print $text;
    print "</a>";
}

function small_header($title) {
  print $dtd;
  print "<html>";
  include('head.phtml');
  include('smallheader.phtml'); 
  print "<h1>" . $title . "</h1>\n</td>\n</table>\n";
}

function small_header_body($title) {
  include('smallheader.phtml'); 
  print "<h1>" . $title . "</h1>\n</td>\n</table>\n";
/*  print "<p>";  FIXME: hack to get CSS to work with bad HTML from texi2html */
}

/* FIXME: remove this function: maybe just set a global variable,
   or use SCRIPT_NAME, and then include footer.phtml. */

function footer($filemodified=".") {
  include('footer.phtml'); 
  date_modified($filemodified);
  print "</address>\n";
//  print "</font>\n";  /* Naughty stuff for older browsers, shouldn't do if V4 */
  print "</body>\n";
  print "</html>\n";
}

function click_to_go_back() {
  print "<p>\nClick <a href=\"index.phtml\">here</a> to go back to the Proof General front page.</p>\n";
}

/* Link to a marked up file */
/* NB: could determine type automatically! */

function fileshow($filename,$text="",$title="") {
 if ( $text == "") { $text=$filename; };
 $message=$title;
 if ( $message == "") { $message=$filename; };
 hlink("fileshow.phtml?file=" . urlencode($filename)
       . "&title=" . urlencode($title),
	$text, $message);
}


/* Similar for html file (NB: could pick automatically) */

function htmlshow($filename,$text="",$title="") {
 if ( $text == "") { $text=$filename; };
 $message=$title;
 if ( $message == "") { $message=$filename; };
 $urlbits = parse_url($filename);
 hlink("htmlshow.phtml"
       . "?title=" . urlencode($title) 
       . "&file=" . urlencode($urlbits["path"])
       . ($urlbits["fragment"]=="" ? "" : "#" . $urlbits["fragment"]),
	$text, $message);
}


/* Markup plain text file, by escaping < and > */

function markup_plain_text($filename) {
  $file = file($filename);
  for($i = 0;$i < count($file);$i++) {
     $line = $file[$i];
     $line = ereg_replace("<","&lt;",$line);
     $line = ereg_replace(">","&gt;",$line);
     print $line;
  };
}

/* Hack an html file to be shown with our style sheet
   and hack relative links to go via htmlshow.phtml. 
   This isn't particularly robust, but seems to work for
   the output of texi2html.
*/

function hack_html($filename,$title="")  {
  if ($title=="") { $title=$filename; };
  $file = file($filename);
  /* Paste style sheet in head */
  for($i = 0;$i < count($file);$i++) {
     $line = $file[$i];
     if (eregi("</HEAD>",$line)) {
        /* Paste in style sheet */
	print "<style type=\"text/css\">\n<!--";
	include("proofgen.css");
	print "-->\n</style>\n";
        /* End style sheet paste in */
        print $line;
	$i++;
	break;
     } else { print $line; };
  }
  /* Now parse rest of document, hacking relative links. */
  /* Matching here is not great, will get confused by html tags inside strings, etc. */
  for (;$i < count($file);$i++)  {
     $line = $file[$i];
       /* PHP has no way to get the match position, so we have to 
	  match a suffix of the line, add extra parenthesis, and calculate it. */
     while (eregi("(<A([^>]*)(HREF=\"([^\"]+)\")(.*))",$line,$linebits)) { 
       /* found URL in a particularly simple form */
       $matchpos = strlen($line) - strlen($linebits[1]);
       print substr($line,0,$matchpos);		/* start of line */
       $line = $linebits[5];			/* rest of line after link */
       $urlbits = parse_url($linebits[4]);
       if ($urlbits["host"]=="") { 
	  /* Assume a relative link, let's hack it. */
	  /* Use same title */
	  $newurl = "htmlshow.phtml?title=" . urlencode($title);
	  if ($urlbits["path"]=="") {
	     /* A fragment in same file */
	     $newurl = $newurl . "&file=" 
	       . urlencode($filename) . "#" . $urlbits["fragment"];
	  } else {
	     /* Another file */
	     $newurl = $newurl . "&file=" 
	    	. urlencode(dirname($filename) . "/" . $urlbits["path"])
		. ($urlbits["fragment"]=="" ? "" : "#" . $urlbits["fragment"]);
	  };
	  print "<A " . $linebits[2] . " HREF=\"" . $newurl . "\"";
	} else { print "<A " . $linebits[2] . $linebits[3]; }
     };
     /* Hack on a header and footer */
     if (eregi("<BODY.*>",$line)) { 
       /* Assume there's nothing else interesting on the line, whoops. */
       print $line;
       small_header_body($title);
     } elseif (eregi("</BODY>",$line)) {
       /* Assume there's nothing else interesting on the line, whoops. */
       click_to_go_back();
       print $line;
     } else {
       print $line;
     };
  }
}


