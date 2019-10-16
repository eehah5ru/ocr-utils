#lang racket
;;;
;;; combining left and right pages in one dir and rename files
;;;

;;
;;
;; todo:
;; general:
;; check the same count of pages in left and rigth dir
;; left pages:
;; - if horizontal - flip 90 CCW
;; - reverse order
;; right pages:
;; - do nothing
;; - rename
;;

(define (escape-for-cmd a-string)
  (string-append "\""
                 a-string
                 "\""))

(define (path->escaped-string a-path)
  (escape-for-cmd (path->string a-path)))


(define (rotate-90-ccw in-file-path out-file-path)
  (let ([cmd (string-join (list "convert"
                          "-rotate"
                          (escape-for-cmd "-90")
                          (path->escaped-string in-file-path)
                          (path->escaped-string out-file-path)))])
    (unless (system cmd)
      (error "error executing command" cmd))))


(define (file-list-in-dir dir-name)
  (parameterize ([current-directory dir-name])
    (for/list ([f-path (in-directory)])
      (build-path dir-name f-path))))

(define (left-page-pathes left-dir)
  (reverse (file-list-in-dir left-dir)))

(define (right-page-pathes right-dir)
  (file-list-in-dir right-dir))


(define (mk-dst-path dst-dir src-path dst-num)
  (let* ([src-ext (bytes->string/utf-8 (path-get-extension src-path))]
         [dst-file-name (string-append (number->string dst-num) src-ext)])
    (build-path dst-dir dst-file-name)))


(define (combine-pages left-dir right-dir out-dir)
  (let* ([left-pages (left-page-pathes left-dir)]
         [right-pages (right-page-pathes right-dir)]
         [page-count (+ (length left-pages) (length right-pages))])

    (unless (= (length left-pages) (length right-pages))
      (error "different count of pages!"))

    ;; copy left pages
    (map rotate-90-ccw
         left-pages                     ; src
         (map (curry mk-dst-path out-dir)  ; dst
              left-pages
              (range 1 page-count 2)))

    ;; copy rigth pages
    (map rotate-90-ccw
         right-pages
         (map (curry mk-dst-path out-dir)
              right-pages
              (range 0 page-count 2)))))
