Pro DCD_pic2, bor = bor, ang = ang

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

	a = [0.5,0]
	b = a + [-lon,tra]
	c = [-0.6,0]
	d = [1,0]*b
	plots, [[a+0.1*(a-c)],[a]], thi=2
	plots, [[a],[c]], thi=2, line = 2
	plots, [[a],[b]], thi=2
	plots, [[b],[b+0.4*(b-a)]], thi=2, line=2
	plots, [[b],[c]], thi = 2
	plots, [[b],[d]], line=1, thi=2

	Arc_mm, a + 0.16*(c-a), cen=a, ang = -2*ftet, line=1, thi=2
	Arc_mm, b + 0.19*(c-b), cen=b, ang = -2*stet, line=1, thi=2
	Arc_mm, c + 0.35*(a-c), cen=c, ang = -2*(ftet-stet), line=1, thi=2
	plots, [[d+[-0.08,0]],[d+[-0.08,0.08]]], line=1,thi=2
	plots, [[d+[-0.08,0.08]],[d+[0,0.08]]], line=1,thi=2

	Arro, from = a+0.1*(a-c), to = a+0.03*(a-c), thi=2, size=1.5
	Arro, from = a, to = 0.5*(a+b), thi=2, size=1.5
	Arro, from = b, to = 0.5*(b+c), thi=2, size=1.5

	Arro, from = a - [0,0.15], to =  c - [0,0.15], thi=2, line=1, size=1.3, /two
	Arro, from = c+[0.18,0.45], to = c + [0.,0.45], thi=1, size=1.
	Arro, from = c+[0.18,0.45], to = c + [0.18,0.63], thi=1, size=1

	Labels, [0.36,-0.02,-0.49],[0.025,0.47,0.025], charsize=1.8,charthi=1.6, $
		['2!7u!d1!n!x','2!7u!d2!n!x','2(!7u!d2!n-!7u!d1!n)!x']
	Labels, [0.5,0.13,-0.62,0.1],[-0.05,0.48,-0.05,-0.05], $
		charsize=1.6,charthi=1.6, ['A','B','C','D']
	Labels, [-0.05,0.14,0.25],[-0.21,0.2,-0.07], charsize=1.6,charthi=1.6, $
		['L','t_off','l_off']
	Labels, [-0.4,-0.52],[0.53,0.41], charsize=1.4, charthi=1.5, ['x','y']
	return
end