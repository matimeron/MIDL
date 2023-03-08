Pro DCD_pic, bor = bor, ang = ang

	on_error, 1

	Box, [-0.7,0.7], [-0.4,0.8], truasp = 'ycen', border=bor
	dftet = ang
	ftet = dftet*!dtor
	ene = Mono_energy(dftet,crys='ge',ind=[1,1,1])
	dstet = Bragg_angle(ene=ene,crys='ge',ind=[2,2,0])
	stet = dstet*!dtor
	plen = sin(2*(stet-ftet))/sin(2*stet)
	lon = plen*cos(2*ftet)
	tra = plen*sin(2*ftet)

	a = [-0.5,0]
	b = a + [lon,tra]
	c = [0.5,0]
	plots, [[a+0.1*(a-c)],[a]], thi=2
	plots, [[a],[c]], thi=2, line = 2
	plots, [[a],[b]], thi=2
	plots, [[b],[b+0.4*(b-a)]], thi=2, line=2
	plots, [[b],[c]], thi = 2

	Arc_mm, a + 0.2*(c-a), cen=a, ang = 2*ftet, line=1, thi=2
	Arc_mm, b + 0.25*(c-b), cen=b, ang = 2*stet, line=1, thi=2
	Arc_mm, c + 0.4*(a-c), cen=c, ang = 2*(ftet-stet), line=1, thi=2

	Arro, from = a+0.1*(a-c), to = a+0.03*(a-c), thi=2, size=1.5
	Arro, from = a, to = 0.5*(a+b), thi=2, size=1.5
	Arro, from = b, to = 0.5*(b+c), thi=2, size=1.5

	Arro, from = a - [0,0.2], to =  c - [0,0.2], thi=2, line=1, size=1.3, /two
	Arro, from = [a[0]-0.2,0], to=  [a[0]-0.2,b[1]], thi=2, line=1, size=1.3,$
	/two
	Arro, from = [a[0],b[1]+0.2], to= [b[0],b[1]+0.2], thi=2, line=1, size=1.3,$
	/two
	Arro, from = c+[0,0.45], to = c + [0.18,0.45], thi=1, size=1.
	Arro, from = c+[0,0.45], to = c + [0,0.27], thi=1, size=1
	Labels, [-0.41,-0.05,0.12],[0.025,0.32,0.025], charsize=1.8,charthi=1.6, $
		['2!7u!d1!n!x','2!7u!d2!n!x','2(!7u!d2!n-!7u!d1!n)!x']
	Labels, [-0.52,-0.15,0.5],[-0.05,0.35,-0.05], charsize=1.6,charthi=1.6, $
		['A','B','C']
	Labels, [-0.01,-0.67,-0.35],[-0.26,0.15,0.55], charsize=1.6,charthi=1.6, $
		['L','t_off','l_off']
	Labels, [0.52,0.57],[0.35,0.48], charsize=1.4, charthi=1.5, ['x','y']
	return
end