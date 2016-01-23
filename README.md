# templateify

Replace all subtree sequences without containers with variables. 
Store a map of those variables to their respective content.

type should be.
templateify :: String -> (String,[String])

```
*Main> templateify "<div> hello <div> world </div> </div>"                                                                                                            
("<div>{{area1}<div>{{area2}</div> </div>",[" world "," hello "])  
```
