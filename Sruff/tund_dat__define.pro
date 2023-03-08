Pro TUND_dat__define

	on_error, 1

	dum = {tund_dat, sync: '', per: 0., npr: 0, tap: 0., harm: 0, eharm: 0., $
	bw: 0., rgam: 0., cur: 0., rsig: fltarr(2), asig: fltarr(2), $     
	ene: ptr_new(), gtxy: ptr_new(), dat: ptr_new(), nrmdat: ptr_new()}

	return
end