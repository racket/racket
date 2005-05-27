;; TeachPack: docs.ss
;; Language: Beginner 

(annotation? '<html>)
(not (annotation? 'html))
(annotation? '<p>)

(eq? '</html> (end-annotation '<html>))

(write-file
 (list '<p> 'hello 'world 'is 'the 'most 'stupid 'program 'in 'the 'world '</p>
       "so let's test this" 'with "How's that"))