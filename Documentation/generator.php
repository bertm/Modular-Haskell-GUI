#!/usr/bin/php
<?php
/*
 * Documentation generator class.
 */

class DocumentationGenerator
{
    private $baseDir;
    private $outputDir;
    private $tplDir;
    private $exclude;
    private $include;
    private $vars;
    
    private $classes;
    private $enums;
    
    /*
     * Public methods.
     */

    public function __construct($config)
    {
        $this->baseDir   = $config['base-dir'];
        $this->outputDir = $config['output-dir'];
        $this->tplDir    = isset($config['tpl-dir']) ? $config['tpl-dir'] : 'template';
        $this->exclude   = isset($config['exclude']) ? $config['exclude'] : '/^$/';
        $this->include   = isset($config['include']) ? $config['include'] : '/\.js$/i';
        $this->vars      = isset($config['vars'])    ? $config['vars']    : array();
        
        $this->classes = array();
        $this->enums   = array();
    }
    
    public function generate()
    {
        $this->parse();
        $this->document();
    }
    
    /*
     * Parsing.
     */

    private function parse()
    {
        // Fetch all files.
        $files = $this->fetchFiles($this->baseDir);

        // Parse files.
        foreach ($files as $file)
        {
            $this->parseFile($file);
        }

        // Sort them.
        ksort($this->classes);
        ksort($this->enums);
    }
    
    // Fetches files recursively.
    private function fetchFiles($dir)
    {
        $files = array();
        foreach (scandir($dir) as $file)
        {
            if (($file == '.') || ($file == '..'))
            {
                continue;
            }
            
            if (is_dir($dir . '/' . $file))
            {
                $files = array_merge($files, $this->fetchFiles($dir . '/' . $file));
            }
            else if (!preg_match($this->exclude, $dir . '/' . $file) && preg_match($this->include, $dir . '/' . $file))
            {
                $files[] = $dir . '/' . $file;
            }
        }
        
        return $files;
    }

    private function parseFile($file)
    {
        // Fetch file content.
        $content = file_get_contents($file);
        
        // Remove all normal comments.
        $content = preg_replace('#\r#m', "\n", $content);
        $content = preg_replace('#/\*[^*](\*[^/]|[^*]+)*\*/#m', '', $content);
        $content = preg_replace('#//[^\n]*#', '', $content);
        
        // Create result and state.
        $state = array();
        
        // Parse file.
        $length = strlen($content);
        for ($i = 0; $i < $length; ++$i)
        {
            switch ($content[$i])
            {
                case '"':
                case '\'':
                    $delimiter = $content[$i];
                    $start     = $i;
                    
                    // Skip string.
                    ++$i;
                    while (($i < $length) && ($content[$i] != $delimiter))
                    {
                         if ($content[$i] == '\\')
                            $i += 2;
                         else
                            ++$i;
                    }
                    
                    // Check for property.
                    $next = substr($content, $i + 1, 256);
                    if (preg_match('/^\s*\:/', $next))
                    {
                        // Handle property.
                        $property = preg_replace('/\\\\(.)/', '\1', substr($content, $start + 1, $i - $start - 1));
                        
                        $this->handleProperty($property, $state);
                        
                        continue 2;
                    }
                    
                    // Handle string.
                    $this->handleRest($state);
                    
                    continue 2;
                    
                case 'C':
                case 'S':
                    // Check for class name.
                    $next = substr($content, $i, 256);
                    if (preg_match('/^(Singleton|Class)\.define\s*\(\s*(\'[^\'\\\\]+\'|"[^"\\\\]+")/', $next, $matches))
                    {
                        $class = str_replace(array('\'', '"'), array('', ''), $matches[2]);
                        
                        echo $class . "\n";
                        
                        // Handle class.
                        $this->handleClass($class, ($matches[1] === 'Singleton'), $state);
                        
                        // Skip class.
                        $i += strlen($matches[0]) - 1;
                    
                        continue 2;
                    }
                    
                    break;
                    
                case 'e':
                    // Check for extend.
                    $next = substr($content, $i, 256);
                    if (preg_match('/^extend\s*:\s*(\'[^\'\\\\]+\'|"[^"\\\\]+")/', $next, $matches))
                    {
                        $extend = str_replace(array('\'', '"'), array('', ''), $matches[1]);
                        
                        // Handle extend.
                        $this->handleExtend($extend, $state);
                        
                        // Skip extend.
                        $i += strlen($matches[0]) - 1;
                        
                        continue 2;
                    }
                    
                    break;
                    
                case '/':
                    if (substr($content, $i, 3) != '/**')
                    {
                        break;
                    }
                    
                    // Skip comment.
                    $start = $i;
                    
                    $i += 3;
                    while (($i < $length) && (substr($content, $i, 2) != '*/'))
                    {
                         ++$i;
                    }
                    
                    // Handle comment.
                    $comment = preg_replace('/(^|\n)\s*\*[ ]?/', '\1', trim(substr($content, $start + 3, $i - $start - 3)));
                    
                    $this->handleComment($comment, $state);
                    
                    $i += 2;
                    
                    continue 2;
                
                case ' ':
                case "\t":
                case "\n":
                    continue 2;
            }
            
            if (ctype_alpha($content[$i]) || ($content[$i] == '_'))
            {
                $start = $i;
                
                // Parse property.
                ++$i;
                while (($i < $length) && (ctype_alnum($content[$i]) || ($content[$i] == '_')))
                {
                    ++$i;
                }
                
                // Check for property.
                $next = substr($content, $i, 256);
                if (preg_match('/^\s*\:/', $next))
                {
                    // Handle property.
                    $property = substr($content, $start, $i - $start);
                    
                    $this->handleProperty($property, $state);
                    
                    continue;
                }
            }
            
            // Handle the rest.
            $this->handleRest($state);
        }
        
        // Merge classes.
        if (isset($state['classes']))
            $this->classes = array_merge($this->classes, $state['classes']);
        
        // Merge enums.
        if (isset($state['enums']))
            $this->enums = array_merge($this->enums, $state['enums']);
    }
    
    /*
     * State helper methods.
     */

    private function handleProperty($property, &$state)
    {
        if ($property == 'properties')
        {
            $state['part'] = 'properties';
            return;
        }
        if ($property == 'actions')
        {
            $state['part'] = 'actions';
            return;
        }
        
        if (isset($state['class']) && isset($state['comment']) && isset($state['part']))
        {
            if (!isset($state['class'][$state['part']]))
                $state['class'][$state['part']] = array();
            
            $state['class'][$state['part']][$property] = array(
                'comment' => $state['comment']
            );
            
            unset($state['comment']);
        }
    }

    private function handleExtend($extend, &$state)
    {
        if (isset($state['class']))
        {
            $state['class']['extend'] = $extend;
        }
    }

    private function handleComment($comment, &$state)
    {
        if (!$comment)
            return;
        
        $lines = explode("\n", $comment);
        
        $comment = array('tags' => array());
        
        $section = &$comment['shortdesc'];
        $inShortDesc = true;
        for ($i = 0; $i < count($lines); ++$i)
        {
            $line = $lines[$i];
            if (preg_match('/^\s*@([\w-]+)\s*(.*)$/', $line, $matches))
            {
                $name = $matches[1];
                if (!isset($comment['tags'][$name]))
                    $comment['tags'][$name] = array();
                
                $comment['tags'][$name][] = array('name' => $matches[1], 'rest' => $matches[2]);
                $section = &$comment['tags'][$name][count($comment['tags'][$name]) - 1]['rest'];
            }
            else if ($inShortDesc && !trim($line))
            {
                $section = &$comment['longdesc'];
                $inShortDesc = false;
            }
            else
            {
                // Add it to section.
                if (!isset($section))
                    $section = $line;
                else
                    $section .= "\n" . $line;
            }
        }
        
        if (!isset($comment['longdesc']))
            $comment['longdesc'] = $comment['shortdesc'];
        
        $state['comment'] = $comment;
    }

    private function handleClass($class, $singleton, &$state)
    {
        if (!isset($state['classes']))
            $state['classes'] = array($class => array());
        else
            $state['classes'][$class] = array();
        
        $state['class'] = &$state['classes'][$class];
        $state['class']['extend'] = $singleton ? 'Singleton' : 'Instance';
        
        if (isset($state['comment']))
        {
            $state['class']['comment'] = $state['comment'];
            unset($state['comment']);
        }
    }

    private function handleRest(&$state)
    {
        unset($state['comment']);
    }
    
    /*
     * Documentation methods.
     */
    
    private function document()
    {
        // Create output dir and copy template to it.
        @mkdir($this->outputDir, 0755, true);
        $this->copyDir($this->tplDir, $this->outputDir, '/\.tpl$/i');
        
        // Document classes.
        $this->documentClasses();
    }
    
    private function copyDir($from, $to, $exclude = '/^$/')
    {
        foreach (scandir($from) as $file)
        {
            if ($file[0] !== '.')
            {
                if (is_dir($from . '/' . $file))
                {
                    @mkdir($to . '/' . $file, 0755);
                    $this->copyDir($from . '/' . $file, $to . '/' . $file, $exclude);
                }
                else if (!preg_match($exclude, $from . '/' . $file))
                {
                    copy($from . '/' . $file, $to . '/' . $file);
                }
            }
        }
    }
    
    private function documentClasses()
    {
        // Document hierarchy.
        $this->documentHierarchy();
        
        // Document each class.
        foreach ($this->classes as $name => $class)
        {
            $this->documentClass($name);
        }
    }
    
    private function documentHierarchy()
    {
        $classes = $this->classes;
        $self = $this;
        $recurse = function($myName) use ($self, &$body, $classes, &$recurse)
            {
                $body .= '<li>' . $self->createLink($myName, '', false);
                
                $hasSubClasses = false;
                foreach ($classes as $name => $class)
                {
                    if ($class['extend'] === $myName)
                    {
                        if (!$hasSubClasses)
                        {
                            $hasSubClasses = true;
                            $body .= '<ul>';
                        }
                        
                        $recurse($name);
                    }
                }
                
                if ($hasSubClasses)
                    $body .= '</ul>';
                
                $body .= '</li>';
            };
        
        $body  = '<div id="title"><h1>Class Hierarchy</h1>';
        $body .= '<div class="hierarchy"><ul>';
        
        foreach ($this->classes as $name => $class)
        {
            if (!isset($this->classes[$class['extend']]))
            {
                $recurse($name);
            }
        }
        
        $body .= '</ul></div>';
        
        $this->writeFile('index', $body, array(
            'title' => 'Class Hierarchy'
        ));
    }
    
    private function documentClass($name)
    {
        // Fetch class.
        $class = &$this->classes[$name];
        
        // Title.
        $body = '<div id="title"><h1>' . htmlspecialchars($name) . '</h1>';
        if (isset($class['comment']['shortdesc']))
            $body .= '<div class="description">' . $this->formatMarkDown($class['comment']['shortdesc'], $name) . '</div>';
        
        $body .= '</div>';
        
        // Object hierarchy.
        $body .= '<div id="hierarchy"><h2>Object Hierarchy</h2>';
        
        // Add class itself.
        $hierarchy = '<ul><li class="current-class">' . htmlspecialchars($name) . '</li>';
        
        // List all direct subclasses.
        $subHierarchy = '';
        foreach ($this->classes as $someClassName => $someClass)
        {
            if (isset($someClass['extend']) && ($someClass['extend'] == $name))
            {
                $subHierarchy .= '<li class="sub-class">' . $this->createLink($someClassName, '', false) . '</li>';
            }
        }
        
        $hierarchy .= ($subHierarchy ? '<ul>' . $subHierarchy . '</ul>' : '') . '</ul>';
        
        // Add parent classes.
        $currentClass = $class;
        while (isset($this->classes[$currentClass['extend']]))
        {
            $hierarchy = '<ul><li class="parent-class">' .  $this->createLink($currentClass['extend'], '', false) . '</li>'
                       . $hierarchy
                       . '</ul>';
            
            $currentClass = $this->classes[$currentClass['extend']];
        }
        
        $body .=  $hierarchy;
        $body .= '</div>';
        
        // Property synopsis.
        if (isset($class['properties']))
        {
            $body .= '<div id="properties"><h2>Properties</h2>';
            
            $body .= '<table>';
            foreach ($class['properties'] as $propertyName => $property)
            {
                $escapedName = htmlspecialchars($propertyName);
                
                $body .= '<tr><td class="name"><a href="#p-' . $escapedName . '">' . $escapedName . '</td>';
                
                if (isset($property['comment']['shortdesc']))
                    $body .= '<td class="description">' . $this->formatMarkDown($property['comment']['shortdesc'], $name) . '</td>';
                
                $body .= '</tr>';
            }
            $body .= '</table></div>';
        }
        
        // Action synopsis.
        if (isset($class['actions']))
        {
            $body .= '<div id="actions"><h2>Actions</h2><table>';
            
            foreach ($class['actions'] as $actionName => $action)
            {
                $escapedName = htmlspecialchars($actionName);
                
                $body .= '<tr><td class="name"><a href="#a-' . $escapedName . '">' . $escapedName . '()</td>';
                
                if (isset($action['comment']['shortdesc']))
                    $body .= '<td class="description">' . $this->formatMarkDown($action['comment']['shortdesc'], $name) . '</td>';
                
                $body .= '</tr>';
            }
            
            $body .= '</table></div>';
        }
        
        // Signal synopsis.
        if (isset($class['signals']))
        {
            $body .= '<div id="signals"><h2>Signals</h2><table>';
            
            $body .= '</table></div>';
        }
        
        // Description.
        $body .= '<div id="description"><h2>Description</h2>';
        if (isset($class['comment']['longdesc']))
            $body .= '<div class="description">' . $this->formatMarkDown($class['comment']['longdesc'], $name) . '</div>';
        $body .= '</div>';
        
        // Details.
        $body .= '<div id="details"><h2>Details</h2>';
        
        // Properties.
        if (isset($class['properties']))
        {
            foreach ($class['properties'] as $propertyName => $property)
            {
                $body .= '<div class="action" id="p-' . htmlspecialchars($propertyName) . '">';
                $body .= '<h3>' . htmlspecialchars($propertyName) . '</h3>';
                
                if (isset($property['comment']['longdesc']))
                    $body .= '<div class="description">' . $this->formatMarkDown($property['comment']['longdesc'], $name) . '</div>';
                
                $body .= '</div>';
            }
        }
        
        // Actions
        if (isset($class['actions']))
        {
            foreach ($class['actions'] as $actionName => $action)
            {
                $body .= '<div class="action" id="a-' . htmlspecialchars($actionName) . '">';
                $body .= '<h3>' . htmlspecialchars($actionName) . '()</h3>';
                
                if (isset($action['comment']['longdesc']))
                    $body .= '<div class="description">' . $this->formatMarkDown($action['comment']['longdesc'], $name) . '</div>';
                
                $body .= '</div>';
            }
        }
        
        $body .= '</div>';
        
        // DEBUG: Add info.
        $body .= '<h2>Debug Info</h2>';
        $body .= '<pre>' . print_r($class, true) . '</pre>';
        
        // Write to file.
        $this->writeFile($name, $body, array(
            'title' => $name
        ));
    }
    
    // Writes a file.
    public function writeFile($name, $content, $vars = array())
    {
        // Add header and footer.
        $content = $this->formatTemplate(file_get_contents($this->tplDir . '/header.tpl'), $vars)
                 . $content
                 . $this->formatTemplate(file_get_contents($this->tplDir . '/footer.tpl'), $vars);
        
        // Write it to file.
        file_put_contents($this->outputDir . '/' . $name . '.html', $content);
    }

    // Creates a link to something.
    public function createLink($name, $context = '', $useCode = true)
    {
        /*
         * Name may be:
         * Class
         * Class.action()
         * Class.property
         * Class.signal  syntax?
         * action()
         * property
         * signal  syntax?
         */
        
        $prefix  = $useCode ? '<code>' : '';
        $postfix = $useCode ? '</code>' : '';
        
        $escapedName = htmlspecialchars($name);
        $isAction = (substr($name, -2) == '()');
        if ($isAction)
        {
            $name = substr($name, 0, -2);
            if (isset($this->classes[$context]['actions'][$name]))
            {
                return $prefix . '<a href="#a-' . htmlspecialchars($name) . '" title="' . $escapedName . ' action">' .
                    $escapedName . '</a>' . $postfix;
            }
        }
        else if (isset($this->classes[$context]['properties'][$name]))
        {
            return $prefix . '<a href="#p-' . $escapedName . '" title="' . $escapedName . ' property">' .
                $escapedName . '</a>' . $postfix;
        }
        else if (isset($this->classes[$name]))
        {
            return $prefix . '<a href="' . $escapedName . '.html" title="' . $escapedName . ' class">' .
                $escapedName . '</a>' . $postfix;
        }
        
        // Split name.
        $dotPos = strrpos($name, '.');
        if ($dotPos !== false)
        {
            $class = substr($name, 0, $dotPos);
            $name  = substr($name, $dotPos + 1);
            
            if ($isAction)
            {
                if (isset($this->classes[$class]['actions'][$name]))
                {
                    return $prefix . '<a href="' . $class . '.html#a-' . htmlspecialchars($name) . '" title="' .
                        $escapedName . ' action">' . $escapedName . '</a>' . $postfix;
                }
            }
            else
            {
                if (isset($this->classes[$class]['properties'][$name]))
                {
                    return $prefix . '<a href="' . $class . '.html#p-' . $escapedName . '" title="' .
                        $escapedName . ' property">' . $escapedName . '</a>' . $postfix;
                }
            }
        }
        
        return '<i>' . $escapedName . '</i>';
    }
    
    // Replaces references with links.
    private function replaceReferences($text, $context = '')
    {
        $self = $this;
        return preg_replace_callback('/\#([\w+\-\.]*\w(\(\))?)/',
            function($matches) use ($self, $context)
            {
                return $self->createLink($matches[1], $context);
            },
            $text);
    }

    // Applies markdown.
    private function formatMarkDown($text, $context = '')
    {
        require_once 'markdown.php';
        
        return $this->replaceReferences(Markdown($text), $context);
    }
    
    // Formats a template.
    private function formatTemplate($content, $vars = array())
    {
        $vars = array_merge($this->vars, $vars);
        return preg_replace_callback('/\{([\w-]+)\}/', function($matches) use ($vars)
        {
            return isset($vars[$matches[1]]) ? htmlspecialchars($vars[$matches[1]]) : '';
        }, $content);
    }
};

// Unset time limit.
set_time_limit(0);

// Configuration.
$config = array(
    'base-dir' => '../Client',
    'output-dir' => 'output',
    'exclude' => '/(resources|external)/i',
    'vars' => array(
        'main-title' => 'Widget library',
    )
);

// Document.
$docGen = new DocumentationGenerator($config);
$docGen->generate();
