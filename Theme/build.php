#!/usr/bin/php
<?php
/*
 * Input.
 */

// Foreground color.
$foregroundColor = array('r' => 0x5a, 'g' => 0x96, 'b' => 0xce); //light blue
$foregroundColor = array('r' => 0xd0, 'g' => 0x17, 'b' => 0x18); //red
$foregroundColor = array('r' => 0xb7, 'g' => 0x3b, 'b' => 0xe4); //purple
$foregroundColor = array('r' => 0x3b, 'g' => 0x4a, 'b' => 0xe4); //dark blue
$foregroundColor = array('r' => 0x77, 'g' => 0xd5, 'b' => 0x3e); //green
$foregroundColor = array('r' => 0xe4, 'g' => 0x7e, 'b' => 0x3b); //orange

// Base color of images.
$baseForegroundColor = array('r' => 0x5A, 'g' => 0x96, 'b' => 0xCE);

// Output folder.
$outputDir = '../Client/themes/clearlooks';

/*
 * Build system.
 */

// Color helper methods.
function rgb2hsv($color)
{
    list($r, $g, $b) = array($color['r'] / 255, $color['g'] / 255, $color['b'] / 255);
    
    $v = max($r, $g, $b);
    $t = min($r, $g, $b);
    $s = ($v == 0) ? 0 : ($v - $t) / $v;
    if ($s == 0)
        $h = -1;
    else
    {
        $a  = $v - $t;
        $cr = ($v - $r) / $a;
        $cg = ($v - $g) / $a;
        $cb = ($v - $b) / $a;
        $h  = ($r == $v) ? $cb - $cg : (($g == $v) ? 2 + $cr - $cb : (($b == $v) ? $h = 4 + $cg - $cr : 0));
        $h  = 60 * $h;
        $h  = ($h < 0) ? $h + 360 : $h;
    }
    
    return array('h' => $h, 's' => $s, 'v' => $v);
}

function hsv2rgb($color)
{
    list($h, $s, $v) = array($color['h'], $color['s'], $color['v']);
    
    if ($s == 0)
    {
        return array('r' => $v, 'g' => $v, 'b' => $v);
    }
    else
    {
        $h    = ($h %= 360) / 60;
        $i    = floor($h);
        $f    = $h - $i;
        $q[0] = $q[1] = $v * (1 - $s);
        $q[2] = $v * (1 - $s * (1 - $f));
        $q[3] = $q[4] = $v;
        $q[5] = $v * (1 - $s * $f);
        
        return array('r' => $q[($i + 4) % 6] * 255, 'g' => $q[($i + 2) % 6] * 255, 'b' => $q[$i % 6] * 255);
    }
}

// Build helper methods.
function createDirs()
{
    global $outputDir;
    
    @mkdir('build', 0777);
    @mkdir($outputDir, 0755, true);
    @mkdir($outputDir . '/stylesheets', 0755);
    @mkdir($outputDir . '/output/images', 0755);
}

function writeConfigFile()
{
    global $foregroundColor;
    
    $r = $foregroundColor['r'];
    $g = $foregroundColor['g'];
    $b = $foregroundColor['b'];
    
    $content = <<<CONTENT
/*
 * Color definitions.
 */

\$foreground-color: rgb($r, $g, $b);
\$background-color: hsl(39deg, 17%, 77%); /* Yellowish-gray. */
\$shadow-color: hsl(0deg, 0%, 50%); /* Gray. */

\$focus-border-color: #000; /* Black. */
\$dark-text-color: adjust-color(\$background-color, \$lightness: -70%); /* Almost black. */
\$bright-text-color: adjust-color(\$foreground-color, \$lightness: 70%); /* White. */

\$normal-background-color: adjust-color(\$background-color, \$saturation: 7%, \$lightness: 15%);
\$alternate-background-color: lighten(\$normal-background-color, 10%);

/*
 * Other definitions.
 */

\$font-family: 'Trebuchet MS', Tahoma, Arial, sans-serif;
\$font-size: 10.5pt;

CONTENT;
    
    file_put_contents('source/_config.scss', $content);
}

function buildStylesheets()
{
    echo `compass compile .`;
}

function moveStylesheets()
{
    global $outputDir;
    
    foreach (scandir('build/') as $file)
    {
        if (substr($file, -4) == '.css')
        {
            rename('build/' . $file, $outputDir . '/stylesheets/' . $file);
        }
    }
}

function colorAndCopyImage($from, $to)
{
    if (!function_exists('imagecreatefrompng'))
        return copy($from, $to);
    
    global $foregroundColor, $baseForegroundColor;
    
    // Skip .gif for now.
    if (substr($from, -4) != '.png')
        return copy($from, $to);
    
    // Convert color of image.
    $base_hsv = rgb2hsv($baseForegroundColor);
    $replace_hsv = rgb2hsv($foregroundColor);

    $global_diff = array(
        'h' => $replace_hsv['h'] - $base_hsv['h'],
        's' => $replace_hsv['s'] - $base_hsv['s'],
        'v' => $replace_hsv['v'] - $base_hsv['v']
    );

    $image = imagecreatefrompng($from);
    $x_dimension = imagesx($image);
    $y_dimension = imagesy($image);
    $new_image = imagecreatetruecolor($x_dimension, $y_dimension);
    imagealphablending($new_image, false);

    for ($x = 0; $x < $x_dimension; $x++) {
        for ($y = 0; $y < $y_dimension; $y++) {

            $rgb = imagecolorat($image, $x, $y);
            
            $color = array('r' => ($rgb >> 16) & 0xFF, 'g' => ($rgb >> 8) & 0xFF, 'b' => $rgb & 0xFF);
            $alpha = ($rgb >> 24) & 0xFF; 
            
            $color_hsv = rgb2hsv($color);
            
            $diff = array(
                'h' => $color_hsv['h'] - $base_hsv['h'],
                's' => $color_hsv['s'] - $base_hsv['s'],
                'v' => $color_hsv['v'] - $base_hsv['v']
            );
            
            if (abs($diff['h']) < 20) // Maybe not a hard cut off.
            {
                $color_hsv['h'] = ($color_hsv['h'] + $global_diff['h'] + 360) % 360;
                $color_hsv['s'] = max(min($color_hsv['s'] + $global_diff['s'], 1), 0);
                $color_hsv['v'] = max(min($color_hsv['v'] + $global_diff['v'], 1), 0);
                
                $new_color = hsv2rgb($color_hsv);
            }
            else
            {
                $new_color = $color;
            }

            $alloc_color = imagecolorallocatealpha(
                $new_image,
                $new_color['r'],
                $new_color['g'],
                $new_color['b'],
                $alpha
            );
            
            imagesetpixel($new_image, $x, $y, $alloc_color);
        }
    }

    imagesavealpha($new_image, true);
    imagepng($new_image, $to);
}

function colorAndCopyImages($dir = 'images')
{
    global $outputDir;
    
    foreach (scandir('source/' . $dir) as $file)
    {
        if ($file[0] == '.')
            continue;
        
        if (is_dir('source/' . $dir . '/' . $file))
        {
            @mkdir($outputDir . '/' . $dir . '/' . $file . '/', 0755, true);
            colorAndCopyImages($dir . '/' . $file . '/');
        }
        else
        {
            colorAndCopyImage('source/' . $dir . '/' . $file, $outputDir . '/' . $dir . '/' . $file);
        }
    }
}

function removeDirs()
{
    @rmdir('build');
}

function removeConfigFile()
{
    @unlink('source/_config.scss');
}

function build()
{
    createDirs();
    writeConfigFile();
    buildStylesheets();
    moveStylesheets();
    colorAndCopyImages();
}

function cleanup()
{
    removeConfigFile();
    removeDirs();
}

// Build and cleanup.
build();
cleanup();

