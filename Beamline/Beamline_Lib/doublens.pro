Function Doublens, l1= l1, l2= l2, lmid= l12, lhs= lhs, rhs= rhs, f1= f1, f2= f2

	on_error, 1

	l1p = l1 + l12
	l2p = l2 + l12
	ltt = l1 + l2 + l12

	lhs = [[l1^2*[-l2*l12^2,l12*(l2+l2p),-l2p]], $
		   [l1*[2*l2*l12*l1p,-(2*l1p*l2p+l2*l12),ltt]], $
		   [l1p*[-l2*l1p,ltt,0]]]

	rhs = [[l2*l12^2,-l12*(l2+l2p),l2p], $
		   [-2*l2*l12,2*l2p,-1], $
		   [l2,-1,0]]

	case One_of(f1,f2,val=f) of
		-1	:	res = fltarr(3,2)
		0	:	res = $
				[[poleval(f,lhs[0,*]),poleval(f,lhs[1,*]),poleval(f,lhs[2,*])],$
				[poleval(f,rhs[0,*]),poleval(f,rhs[1,*]),poleval(f,rhs[2,*])]]
		1	:	res = $
				[[poleval(f,lhs[*,0]),poleval(f,lhs[*,1]),poleval(f,lhs[*,2])],$
				[poleval(f,rhs[*,0]),poleval(f,rhs[*,1]),poleval(f,rhs[*,2])]]
	endcase

	return, res
end