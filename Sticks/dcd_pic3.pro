Pro DCD_pic3, bor = bor, ang = ang

	on_error, 1

	len = 1200.
	ene = [7.,30]
	
	Plvar_keep, act='sav'

	!x.margin = [4,3]
	!y.margin = [2,2]
	Box, [0,1500], [-200,400], border=bor

	DCD_locs, eran=ene[0],fix='ful',len=len, ful=ful,lon=lon,tra=tra,/no
	off = 100
	a = [len+off,0]
	b = a + [-lon,tra]
	c = a + [-len, 0]
	Arro, from = a + [100,0], to = a, thi = 2, siz =1
	Arro, from = a, to = b, thi = 2, siz = 1
	Arro, from = b, to = c, thi = 2, siz = 1
	plots, [[a],[c]], line=2, thi = 2
	Psyms
	plots, b, psym = -1, symsize = 1.5, thi = 2, col = !pcol.dgreen

	DCD_locs, eran=ene[1],fix='ful',len=len, ful=fful,lon=llon,tra=ttra,/no
	off = 100
	aa = [len+off,0]
	bb = aa + [-llon,ttra]
	cc = aa + [-len, 0]
;	Arro, from = a + [100,0], to = a, thi = 2, siz = 1.5
	Arro, from = aa, to = bb, thi = 2, line = 1, siz = 1
	Arro, from = bb, to = cc, thi = 2, line = 1, siz = 1
;	plots, [[a],[c]], line=2, thi = 2
	Psyms
	plots, bb, psym = -1, symsize = 1.5, thi = 2, col = !pcol.red

	Labels, [1280,820,90,820],[-30,300,-30,30], $
	charsize=1.6,charthi=1.6, ['A','B!d1!n','C','B!d2!n']

	Plvar_keep, act='res'

	return
end