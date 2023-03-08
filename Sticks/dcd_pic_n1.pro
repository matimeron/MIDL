Pro DCD_pic_n1, bor=bor, ang = ang

	on_error, 1

	window, 0, xsize=512, ysize = 640

	Plvar_keep, act='sav'
	!x.margin = [4,4]
	Box, [-0.2,0.6], [-0.1,1.1], truasp = 'xlo', border=bor
	dftet = ang
	ftet = dftet*!dtor
	ene = Mono_energy(dftet,crys='ge',ind=[1,1,1])
	stet = Bragg_angle(ene=ene,crys='ge',ind=[2,2,0],/rad)
	plen = sin(2*(stet-ftet))/sin(2*stet)
	lon = plen*cos(2*ftet)
	tra = plen*sin(2*ftet)

	a = [0,0]
	b = [tra,lon]
	c = [0,1]
	xx = [a[0],b[0],c[0]]
	yy = [a[1],b[1],c[1]]

	plots, a, psym=8, thi=2, symsize=1.5
	plots, b, psym=8, thi=2, symsize=1.5
	plots, c, psym=8, thi=2, symsize=1.5
	plots, a, c, lin=2, thi=2

	Arc_mm, a + 0.2*(c-a), cen=a, ang=-2*ftet, line=1, thi=2
	Arc_mm, b + 0.25*(c-b), cen=b, ang=-2*stet, line=1, thi=2
	Arc_mm, c - 0.45*(c-a), cen=c, ang=2*(stet-ftet), line=1, thi=2

	Arro, from=a, to=b, thi=2, size=2
	Arro, from=b, to=c, thi=2, size=2
	Arro, from=b, to=b+ 0.5*(b-a), thi=2, line=2, siz=0

	s = c + [0.35,-0.2]
	Arro, from= s, to= s+ [0.15,0], thi=1, siz=1
	Arro, from= s, to= s + [0,0.15], thi=1, siz=1

	psyms

	Labels, xx + [0,0.04,0],yy + [-0.05,-0.02,0.03], align=0.5, $
		charsize= 1.6, charthi=1.6, ['A','B','C']
	Labels, xx + [0.02,-0.02,0.01], yy +[0.12,0.1,-0.4], charsize= 1.6, $
		charthi=1.5, ['2!7u!d1!n!x','2!7u!d2!n!x','2(!7u!d2!n-!7u!d1!n)!x']
	Labels,[s[0]+0.1,s[0]-0.03],[s[1]-0.03,s[1]+0.1],charsiz=1.4,charthi=1.5,$
		 ['x','y']

	Plvar_keep, act='res'

	return
end