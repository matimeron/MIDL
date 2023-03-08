Function  Mhash_find, file, smooth= smo, threshold= tre, min= min, max= max, $
	clean = cle, raw = im_arr, _extra=_e

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

	read_jpeg, File_get(file), im_arr, /grayscale
	fsiz = (size(im_arr))[1:2]
	out_arr = Img_features(im_arr,$
		smooth=Default(smo,9),thre=tre,min=min,max=max,clean=cle,ext=ext)

	cols = [!pcol.black,!pcol.white]
	Display_mm, im_arr, /auz, isize = isiz, _extra =_e
	if ext le 0 then contour, out_arr < 0,findgen(fsiz[0]),findgen(fsiz[1]),$
	/noerase,xsty=13,ysty=13,pos=64+[0,0,isiz],/dev,col=cols[0]
	if ext ge 0 then contour, out_arr > 0,findgen(fsiz[0]),findgen(fsiz[1]),$
	/noerase,xsty=13,ysty=13,pos=64+[0,0,isiz],/dev,col=cols[1]

	return, out_arr
end