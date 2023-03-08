Function Poly_filter, x, y, order = ord

	on_error, 1

	n = Split_xy(x,y,x=wx,y=wy)

	typ = Calctype(wx,wy)
	orn = 0 > Default(ord,0,/dtyp)
	if orn ge n then message, 'Order must be lower than data length!'

	res = wy
	v = make_array(n,orn+1,type=typ)
	for i = 0, orn do begin
		v(*,i) = wx^i
		for j = 0, i-1 do v(*,i)= v(*,i)- Integ(wx,v(*,i)*v(*,j),/val)*v(*,j)
		v(*,i) = v(*,i)/sqrt(Integ(wx,v(*,i)^2,/val))
		res = res - Integ(wx,res*v(*,i),/val)*v(*,i)
	endfor

	return, FPU_fix(res)
end