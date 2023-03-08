Pro Scatplot, radius= rad, distance= len, tet= tet, phimax= phm, radians= rds, $
	_extra = _e

	on_error, 1

	if keyword_set(rds) then mult = 1. else mult = !dtor
	wtet = mult*tet
	wphm = mult*phm

	rphi = make_grid([0,wphm],256)
	rp = 2*sin(wtet)*sin(rphi)
	rq = 2*sin(wtet)*cos(rphi)
	rx = rp/(2-rq^2)*(len*rq)
	rrx = rp/(2-rq^2)*(len*rq + rad)
	rz = rq*sqrt(4-rp^2-rq^2)/(2-rq^2)*(len)
	rrz = rq*sqrt(4-rp^2-rq^2)/(2-rq^2)*(len + rad*rq/2)
	rvals = complex(rx,rz)
	rrvals = complex(rrx,rrz)

	lphi = -rphi
	lp = 2*sin(wtet)*sin(lphi)
	lq = 2*sin(wtet)*cos(lphi)
	lx = lp/(2-lq^2)*(len*lq)
	llx = lp/(2-lq^2)*(len*lq + rad)
	lz = lq*sqrt(4-lp^2-lq^2)/(2-lq^2)*(len)
	llz = lq*sqrt(4-lp^2-lq^2)/(2-lq^2)*(len + rad*lq/2)
	lvals = complex(lx,lz)
	llvals = complex(llx,llz)

	window, 0	
	Complexmap, rvals, lvals, line = 2, _extra = _e
	Complexmap, rrvals, llvals, /over, _extra = _e
;	window, 1
;	rvals = complex(rx,rq)
;	lvals = complex(lx,lq)
;	Complexmap, rvals, lvals, _extra = _e
;	print, 1 - min(lq)/max(lq)

	return
end