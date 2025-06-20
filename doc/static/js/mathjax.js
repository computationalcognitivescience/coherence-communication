// From Spark, licensed APL2
// https://github.com/apache/spark/commit/36827ddafeaa7a683362eb8da31065aaff9676d5

function injectMathJax() {
    var script = document.createElement('script');
    script.type = 'text/javascript';
    script.async = true;
    script.onload = function(){
        MathJax.Hub.Config({
            displayAlign: "left",
            tex2jax: {
                inlineMath: [ ["$", "$"], ["\\\\(","\\\\)"] ],
                displayMath: [ ["$$","$$"], ["\\[", "\\]"] ],
                processEscapes: true,
                skipTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'a']
            }
        });
    };
    script.src = ('https:' == document.location.protocol ? 'https://' : 'http://') +
        'cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML';
    document.getElementsByTagName('head')[0].appendChild(script);
}

document.addEventListener('DOMContentLoaded', injectMathJax)
