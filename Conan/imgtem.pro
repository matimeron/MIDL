Function ImgTem, fname, bas2000 = bas2000, bas2500 = bas2500, $
	zoom = zoom, nbits = nbits, sens = sens, lat = lat, info = inf, $
	flip = flip, negative = neg, linear = lin, logout = logo, _extra= _e

; temporary adaption of Dave Cookson's RD_IMAGE routine, for older IDL versions

	on_error, 1
	imtyp = One_of(bas2000,bas2500) > 0

	case imtyp of
		0	:	begin
					bas = 512l
					nbits = Default(nbits,10,/dtyp)
					rsens = 4000.
					sens = Default(sens,10000.,/dtyp)
					lat = Default(lat,4.,/dtyp)
				end
		1	:	begin
					bas = 500l
					nbits = Default(nbits,16,/dtyp)
					rsens = 4000.
					sens = Default(sens,10000.,/dtyp)
					lat = Default(lat,5.,/dtyp)
				end
		else:	message, 'Unknown image type!'
	endcase

	ro = 4*bas
	co = 5*bas
	zoom = Default(zoom,4,/dtyp)

	flipfl = keyword_set(flip)
	linfl = keyword_set(lin)
	negfl = keyword_set(neg)

	img=uintarr(ro,co)

	fname = File_get(fname,status=stat,_extra=_e)
	if stat eq 0 then message, 'Cannot find file!'
	openr, datun, fname, /get_lun, /block
	readu, datun, img
	free_lun, datun
	byteorder,img

	img = rebin(img,ro/zoom,co/zoom)
	if flipfl then img = transpose(img)
	dims = (size(img))[1:2]
	if linfl then begin
		logo = img
		tem = findgen(2^nbits)/2^nbits - 0.5
		lookup = rsens/sens*10^(lat*tem)
		img = lookup(img)
		if keyword_set(neg) then begin
			img = max(img)/img
			logo = max(logo) - logo
		endif
	endif else if negfl then img = max(img)-img

	inf = {imgread,fname,dims,nbits,sens,lat,zoom,flipfl,linfl,negfl}
	return, img
end
