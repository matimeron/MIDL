Pro Show_region, img, imgstruct= imginf, radius= rad, $
	negative = neg, threshold = tre, rescale= rsc, _extra= _e

	on_error, 1

	if Type(imginf) ne 8 then message, 'Image structure is required!'

	if Type(img) eq 0 then begin
		img = ImgTem(imginf.read_inf.file, flip=imginf.read_inf.flip, $
			info = rdinf, _extra=_e)
		scinf = imginf.scan_inf
		scale = rdinf.dims/scinf.dims
		if not Arreq(scale,[1,1]) then if scale[0] eq scale[1] $
		then scinf.cent = scinf.cent*scale else message, 'Scaling problems'
		wimginf = {imgsym, read_inf: rdinf, scan_inf: scinf}
	endif else wimginf = imginf

	wimg = Byte_img(img,size=siz)
	dum = Consec(size=siz,foc=wimginf.scan_inf.foc,cent=wimginf.scan_inf.cent,$
		excent=wimginf.scan_inf.excent,rot=wimginf.scan_inf.rot,rad=rad,/new, $
		count=nin, complement= comp, ncomp =nout, _extra=_e)

	if nin gt 0 then begin
		if n_elements(tre) gt 0 then begin
			tbot = byte((0>tre<1)*max(wimg[dum]))
			wimg = wimg > tbot
		endif else tbot = 0b
		if keyword_set(rsc) then bot = min(wimg[dum]) else bot = min(wimg)
	endif else bot = min(wimg)
	if nout gt 0 then wimg[comp] = bot
	if keyword_set(neg) then wimg = max(wimg) - wimg

	if !d.window eq (-1) then wset
	wshow
	tvscl, wimg

	return
end