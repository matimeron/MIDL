Function DCD_atemp, ene, chi, length = len, degrees = deg, radians = rad

	on_error, 1

	typ = Calctype(ene,chi,0.)
	phi1 = Bragg_angle(ene=1d*ene,crys='ge',ind=[1,1,1],/rad)
	phi2 = Bragg_angle(ene=1d*ene,crys='ge',ind=[2,2,0],/rad)
	if len le 10 then wlen = 1d3*len else wlen = 1d*len
	if One_of(deg,rad) eq 0 then amult = !dpi/180 else amult = 1d
	wchi = amult*chi

	prot = Mat_rot(wchi,ax=[0,1,0])
	nrot = transpose(prot)

	n1 = [cos(phi1),-sin(phi1),0]
	n2 = [-cos(2*phi1-phi2),sin(2*phi1-phi2),0]
	n3 = prot##[0,0,1]

	v0 = prot##[0,1,0]
	v1 = Mat_refl(n1)##v0
	v2 = Mat_refl(n2)##v1

	s01 = Mat_proj(v0,n1,/com)
	s12 = Mat_proj(v1,n2,/com)
	s23 = Mat_proj(v2,n3,/com)

	p0 = prot##[0,-1e-6,0]

	q1 = [0,0,0]
	q2 = wlen*sin(2*(phi2-phi1))/sin(2*phi2)*[sin(2*phi1),cos(2*phi1),0]
	q3 = wlen*[0,1,0]
	
	p1 = q1 + s01##(p0-q1)
	p2 = q2 + s12##(p1-q2)
	p3 = q3 + s23##(p2-q3)

	off = nrot##(p3 - q3)
	yy = [-cos(wchi)*sin(2*(phi2-phi1)),cos(2*(phi2-phi1)),0]
	yy = yy/Vnorm(yy)
	xx = crossp(yy,[0,0,1])
	toff = Vinp(off,xx)
	loff = Vinp(off,yy)

	return, Cast([toff,loff],typ,typ)
end