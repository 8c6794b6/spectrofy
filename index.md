<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8" />
<title>Spectrofy - image to audio converter</title>
</head>
<body>
<div style="display:block; margin-right:auto; margin-left:auto; text-align:center; width=600px;">
<a href="https://github.com/8c6794b6"><img style="position: absolute; top: 0; right: 0; border: 0;" src="https://s3.amazonaws.com/github/ribbons/forkme_right_red_aa0000.png" alt="Fork me on GitHub"></a>

Spectrofy - image to audio converter
====================================

Spectrofy is a simple image to audio converter.

<div>
  <img src="lena_in.jpg" style="display:block;margin-left:auto; margin-right:auto;" />
</div>

Converting above image with invoking:

<code>
  <pre>
      $ spectrofy fft -f512 lena.bmp lena.wav
  </pre>
</code>

result sound:

<audio controls="controls">
  <source src="lena.ogg" type="audio/ogg" />
  <source src="lena.mp3" type="audio/mp3" />
  <a href="lena.mp3"> download </a>
</audio>

spectrogram of the sound:

<div>
  <img src="lena_out.png" style="display:block;margin-left:auto; margin-right:auto;" />
</div>

</div>
</body>
</html>
