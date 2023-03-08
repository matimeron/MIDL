Pro Get_pstop_data, detailed = det, $
	hang = hang, x = x, sx = sx, sy = sy, pow = pow, tilt = tilt, etilt = etilt

	on_error, 1

	fnam = ['pinkstop_new','pinkstop_new_detail']
	efnam = ['pinkstop_new_extended','pinkstop_new_extended_detail']

	dfl = keyword_set(det)

	fil = File_get(fnam[dfl],filt='txt',path=getenv('sruff_data')+'bio\')
	efil = File_get(efnam[dfl],filt='txt',path=getenv('sruff_data')+'bio\')

	tem = Rascii(fil)
	hang = reform(tem[0,*])
	x = reform(tem[1,*])
	sx = reform(tem[2,*])
	sy = reform(tem[3,*])
	pow = reform(tem[4,*])
	tilt = reform(tem[5,*])
	tem = Rascii(efil)
	etilt = reform(tem[5,*])

	return
end