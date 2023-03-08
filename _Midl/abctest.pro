Pro ABCtest, arr = arr, ars = ars, asr = asr, ass = ass, $
	bv0 = bv0, bv1 = bv1, bvr0 = bvr0, bvr1 = bvr1, bvs0 = bvs0, bvs1 = bvs1, $
	c0 = c0, c2 = c2

	on_error, 1

	cards = dindgen(12) + 6
	avec = (Shuffle(cards))[0:3]
	aaa = Diagoarr(avec)

	u = randomn(s,4,4,/doub)
	dum = Gram_Schmidt(u[0,*],u[1,*],u[2,*],u[3,*],$
		v00=fir,v01=sec,v02=thi,v03=fou)
	v= [fir,sec,thi,fou]
	aaa = v##aaa##transpose(v)
	aaa = (aaa + transpose(aaa))/2

	bv0 = (Shuffle(cards))[0:3]/8
	bv1 = (Shuffle(cards))[0:3]/4
	c0 = (Shuffle(cards))[0]/16
	c2 = (Shuffle(cards))[0]/8

	arr = aaa[0:1,0:1]
	ars = aaa[2:3,0:1]
	asr = aaa[0:1,2:3]
	ass = aaa[2:3,2:3]

	bvr0 = bv0[0:1]
	bvs0 = bv0[2:3]
	bvr1 = bv1[0:1]
	bvs1 = bv1[2:3]

	return
end	