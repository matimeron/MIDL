Function Byte_img, img, size = siz

	on_error, 1

	siz = size(img)
	if siz[0] ne 2 then message, 'Not an image!'
	if siz[3] gt 1 then begin
		dim = siz[1:2]
		bot = min(img,max=top)
		if bot lt top then res = byte(round(255./(top-bot)*(img-bot))) $
		else res = byte(Noise(make_array(dim[0],dim[1],val=127b)))
	endif else res = img

	return, res
end