Pro DCD_pic4, bor = bor, ang = ang

	on_error, 1

	len = 1200.
	ene = [7.,30]
	
	Plvar_keep, act='sav'

	!x.margin = [4,3]
	!y.margin = [2,2]
	Box, [834.5,836.5], [59.15,59.25] ;border=0, xstyle=1, ystyle=1

	DCD_locs, eran=ene[0],fix='ful',len=len, ful=ful,lon=lon,tra=tra,/no
	off = 100
	a = [len+off,0]
	b = a + [-lon,tra]
	c = a + [-len, 0]

	DCD_locs, eran=ene[1],fix='ful',len=len, ful=fful,lon=llon,tra=ttra,/no, $
	ftet=ftet, stet=stet
	off = 100
	aa = [len+off,0]
	bb = aa + [-llon,ttra]
	cc = aa + [-len, 0]
	Arro, from = aa, to = bb, thi = 2, siz = 1, noclip=0
	Arro, from = bb, to = cc, thi = 2, siz = 1, noclip = 0
	Psyms

	e = a + [-lon,ttra]
	f = bb + [0,(e[0]-bb[0])*tan(2*ftet-stet)]
	Rectan, xlims = [830,840], ylims = e[1] + [0,0.1], /fil, noclip=0, $
	color=!pcol.lblue, rotate=stet-2*ftet, rotation_cent=e

	plots, bb, psym = -8, symsize = 1.5, thi = 2, col = !pcol.red
	e = a + [-lon,ttra]
	plots, e, psym = -8, symsize = 1.5, thi = 2, col = !pcol.red
	plots, f, psym = -8, symsize = 1.5, thi = 2, col = !pcol.orange
	plots, [[e],[bb]],thi=2,line=2
	plots, [[bb],[f]],thi=2,line=2
	plots, [[f],[e]],thi=2,line=2
	Arc_mm, e + 0.5*(bb-e), cen=e, ang = (stet-2*ftet), line=1, thi=2
	

	Labels, [836.1,834.95,834.95,835.65],[59.192,59.198,59.22,59.201], $
	charsize=1.6,charthi=1.6, ["B!d1!n'",'B!d2!n','E','2!7u!d1!n-!7u!d2!n!x']

	Plvar_keep, act='res'

	return
end