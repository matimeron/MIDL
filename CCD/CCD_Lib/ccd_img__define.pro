Pro CCD_img__define

	common CCD_stuff, exs, nco, nro, lfnam, extr
	on_error, 1

	dum = {CCD_img, fnam: '', info: {ccd_info}, extr: {ccd_extr}, $
			imco: 0l, imro: 0l, $
			xval: fltarr(nco), yval: fltarr(nro), img: ulonarr(nco,nro)}
	return
end