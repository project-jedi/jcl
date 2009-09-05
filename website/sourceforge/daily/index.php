<?php

function GetDisplayFileDate($filename)
{
  if (file_exists($filename))
    return date("Y-m-d H:i:s T", filemtime($filename));
  else
    return "Missing file";
}

function GetDisplayFileSize($filename)
{
  if (!file_exists($filename))
    return "Missing file";

  $size = filesize($filename);

  if ($size > 1024*1024)
    return round($size / (1024*1024), 2)." MB";
  else if ($size > 1024)
    return round($size / 1024, 2)." KB";
  else
    return $size;
}

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1"
 http-equiv="content-type">
  <title>Daily packages</title>
  <link rel="STYLESHEET" type="text/css" href="styles/default.css">
</head>
<body>
<h1>JCL Daily packages
</h1>
<hr style="width: 100%; height: 2px;">
<br>
Welcome to the daily packages website for the JCL.<br>
On this page, you will find archive files containing a snapshot of the
JCL done automatically every night.<br>
They contain a copy of the development repository and as such contain
all the latest bug fixes and improvements provided by the JCL
developers.<br>
<br>
The latest version is available for download below<br>
<br>
<table
 style="width: 75%; text-align: left; margin-left: auto; margin-right: auto;"
 cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; font-weight: bold;">File<br>
      </td>
      <td style="vertical-align: top; font-weight: bold;">Date and time<br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Size</span><br>
      </td>
<!--      <td style="vertical-align: top;"><span style="font-weight: bold;">Description</span><br>
      </td>-->
    </tr>
      <?php

      $dh = opendir("./");
      $filenames = array();
      while (($filename = readdir($dh)) !== false)
      {
        if (!is_dir($filename) && (substr($filename, 0, 4) == "jcl-"))
        {
          $filenames[] = $filename;
        }
      }

      // sort the array before displaying it
      rsort($filenames);

      foreach ($filenames as $filename)
      {
        $filename_full = $filename;
        echo '<tr>';
        echo '  <td style="vertical-align: top;"><a href="'.$filename_full.'">'.$filename_full.'</a><br>';
        echo '  </td>';
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename_full);
        echo '  </td>';
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename_full);
        echo '  </td>';
//        echo '  <td style="vertical-align: top;">The complete set of files<br>';
//        echo '  </td>';
        echo '</tr>';
      }
      ?>
  </tbody>
</table>
<br>
The dates are presented according to the ISO standard (YYYY-MM-DD) and the hours are those of the location of the web server.<br>
<br>
Should you have any problems with those files, please do not hesitate
to contact us on our newsgroup here:<br>
<br>
<a href="news://forums.talkto.net/jedi.jcl">news://forums.talkto.net/jedi.jcl</a><br>
<br>
Thank you for considering the JCL.<br>
<br>
</body>
</html>
