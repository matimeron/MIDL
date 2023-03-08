Pro SCU_dat_proc, dat, gap = gap, per = per, jcr = jcr, bmx = bmx, bgp = bgp

	on_error, 1

	if n_elements(dat) eq 0 then dat = Rascii()
	gap = reform(dat[0,*])
	per = reform(dat[1,*])
	jcr = reform(dat[2,*])
	bmx = reform(dat[3,*])
	bgp = reform(dat[7,*])

	gsor = Sorpurge(gap,net=ngp)
	siz = size(dat,/dim)
	npr = siz[1]/ngp

	gap = gap[gsor]
	per = per[0:npr-1]
	jcr = jcr[0:npr-1]
	bmx = bmx[0:npr-1]
	bgp = transpose(reform(bgp,npr,ngp))

	return
end