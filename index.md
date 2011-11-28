<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Spectrofy - image to audio converter</title>
</head>
<body>
<div style="display:block; margin-right:auto; margin-left:auto; text-align:center; width=600px;">
<a href="http://github.com/8c6794b6"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://a248.e.akamai.net/assets.github.com/img/e6bef7a091f5f3138b8cd40bc3e114258dd68ddf/687474703a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f7265645f6161303030302e706e67" alt="Fork me on GitHub"></a>

Spectrofy - image to audio converter
====================================

Spectrofy is a simple image to audio converter. 

<div>
  <img src="lena_in.bmp" style="display:block;margin-left:auto; margin-right:auto;" />
</div>

It converts input image to audio file with invoking:

<code>
  <pre>
      $ spectrofy fft -f512 lena.bmp lena.wav
  </pre>
</code>

to this sound:

<audio controls="controls" src="lena.wav" preload="auto"></audio>

spectrogram of result looks like this:

<div>
  <img src="lena_out.png" style="display:block;margin-left:auto; margin-right:auto;" />
</div>

</div>
</body>
</html>