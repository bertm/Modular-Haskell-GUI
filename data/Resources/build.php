#!/usr/bin/php
<?php
/*
 * Input.
 */

// Output folder.
$outputDir = '../Client/resources/';

/*
 * Build system.
 */

// Build helper methods.
function createDirs()
{
    global $outputDir;
    
    @mkdir('build', 0777);
    @mkdir($outputDir, 0755, true);
    @mkdir($outputDir . '/stylesheets', 0755);
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

function removeDirs()
{
    @rmdir('build');
}

function build()
{
    createDirs();
    buildStylesheets();
    moveStylesheets();
}

function cleanup()
{
    removeDirs();
}

// Build and cleanup.
build();
cleanup();

