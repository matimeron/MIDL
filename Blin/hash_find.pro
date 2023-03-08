function  hash_find, file, smooth=smo,threshold=thres

;+
;NAME:  hash_find
;  SYNTAX: result=hash_find([file, smooth=smooth, threshold=threshold])
;  INPUTS (none mandatory): 
;    FILE: A full file name for the image to be read. Will be selected interactively if absent. 
;    SMOOTH: The value used for smooothing in smooth_mm. Scalar. Default is 9 for now.
;    THRESHOLD: The value used for the threshold in extrema.pro. Default is 1 for now. 
;    These parameters (smo & thres) will need to be changed from image to image. 
;  OUTPUTS:
;    Returns an array which is the size of the image composed of 0s and 1s. Value is 1
;       if there is hash in that location. 
;  PURPOSE: Finds all local maxima in 1 direction in an image. Returns an array of the same
;       dimensiozn as the input image. Used to find the location of "hash" in the gold images.
;  Written by Brian Leahy, 03/31/09ish. 
;-

on_error, 1
;Finding the file name
if not keyword_set(file) then file=file_get()

;Reading the file
read_jpeg, file, im_arr0, /grayscale
;Smoothing the file
if not keyword_set(smo) then smo=9
im_arr=smooth_mm(im_arr0,smo)

fsiz=fltarr(2)
fsiz=[n_elements(im_arr[*,0]),n_elements(im_arr[0,*])]
out_arr=fltarr(fsiz[0],fsiz[1])

if not keyword_set(thres) then thres=1
;looping over the horizontal
for i=0,fsiz[1]-1 do begin
    im_lin=im_arr[*,i]
    min_loc=extrema(im_lin, /min_only, threshold=thres)
    for j=0,n_elements(min_loc)-1 do begin
        out_arr[min_loc[j]>0,i]=1
    endfor
endfor
;looping over the vertical
for i=0,fsiz[0]-1 do begin
    im_lin=im_arr[i,*]
    min_loc=extrema(transpose(im_lin), /min_only, threshold=thres)
    for j=0,n_elements(min_loc)-1 do begin
       out_arr[i,min_loc[j]>0]=1
    endfor
endfor

window, 0, xsize=fsiz[0]+150, ysize=fsiz[1]+150
dgcont_image, im_arr, /nocontour
contour, out_arr, /noerase, xran=[0,fsiz[0]], yran=[0,fsiz[1]], xstyle=1, ystyle=1
return, out_arr
end
