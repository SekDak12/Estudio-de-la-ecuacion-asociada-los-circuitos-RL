	PROGRAM PROYECTOCOMPUTO
	IMPLICIT NONE
	DOUBLE PRECISION T, Y, H, U, R , L, F, IT, P, w, K1, K2, K3, K4,Ko, Ki, o, gi, gk, q, gl, a, e, j
	Integer v, x, m, z
	WRITE(*,*) "¿Cuales son los valores iniciales de nuestro circuito RL?"
	WRITE(*,*) "DONDE T=TIEMPO, Y=, H=TAMAÑO DE PASO"
	READ(*,*) T, Y ,H
	write(*,*) "Cual problema espera resolver? (1,2,3,4)"
	read(*,*) z
	IT=T
	P=Y
	open(unit=4,file="resultados.txt",status="unknown")
	if (z.eq.1) then 
		WRITE(*,*) "Ahora, ¿Cuales son los valores de U, R y L?"
		WRITE(*,*) "Con U=Potencial, R=Resistencia, L=Inductor"
		READ(*,*) U, R, L
10		DO WHILE (IT.LT.20)
		w=(H/2)
		K1=F(P,U,R,L)
		K2=F(P+w*K1,U, R, L)
		K3=F(P+w*K2,U, R, L)
		K4=F(P+H*K3,U,R,L)
		P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
		IT=IT+H
		WRITE(4,*)IT, P
		endDO  
	else if (z.eq.2) then
		   write(*,*) "Proporcione los valores del potenicial U y la inductancia L"
			read(*,*) U, L
			write(*,*) "Propocione a y n tal que R(t)=a*t**n"
			read(*,*) a, q
			IT=T
			P=Y
			DO WHILE (IT.LT.20)
			w=(H/2)
			K1=j(P,U,a,IT,q,L)
			K2=j(P+w*K1,U, a,IT,q, L)
			K3=j(P+w*K2,U, a,IT,q, L)
			K4=j(P+H*K3,U,a,IT,q,L)
			P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
			IT=IT+H
			WRITE(4,*)IT, P
			endDO  	
	else if (z.eq.3) then
			write(*,*) "Propocione los valores de la resistencia R y la de la inductancia L"
			read(*,*) R, L
			WRITE(*,*) "De acuerdo si en la variacion del Potencial se encuentra Seno (1) o coseno (2) eliga 1 o 2"
			read(*,*) v		
			if(v.eq.1) then 
				write(*,*) "Si el seno se multiplca por t eliga 1, en caso de no ser asi presione cualquier otro numero"
				read(*,*) x
				if(x.eq.1) then
					write(*,*) "Cual es e y r tales que U(t)=e*t*sin(r*t)"
					read(*,*) e, r
					DO WHILE (IT.LT.20)
					w=(H/2)
					K1=gk(P,e,r,IT,R,L)
					K2=gk(P+w*K1,e,r,IT, R, L)
					K3=gk(P+w*K2,e,r,IT, R, L)
					K4=gk(P+H*K3,e,r,IT,R,L)
					P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
					IT=IT+H
					WRITE(4,*)IT, P
					enddo
				else 
					write(*,*) "Cual es e y r tales que U(t)=e*sin(r*t)"
					read(*,*) e, r
					DO WHILE (IT.LT.20)
					w=(H/2)
					K1=gl(P,e,r,IT,R,L)
					K2=gl(P+w*K1,e,r,IT, R, L)
					K3=gl(P+w*K2,e,r,IT, R, L)
					K4=gl(P+H*K3,e,r,IT,R,L)
					P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
					IT=IT+H
					WRITE(4,*)IT, P
					enddo
				endif
			else
				write(*,*) "Cuales son los valores e, r tales que e*(t**2)*cos(r*t)"
				read(*,*) e, r				

				DO WHILE (IT.LT.20)
				w=(H/2)
				K1=gi(P,e,r,IT,R,L)
				K2=gi(P+w*K1,e,r,IT, R, L)
				K3=gi(P+w*K2,e,r,IT, R, L)
				K4=gi(P+H*K3,e,r,IT,R,L)
				P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
				IT=IT+H
				WRITE(4,*)IT, P
				enddo
			endif
	else		
		write(*,*) "Eliga 1, 2, 3, respecticamente si su potencial varia"
		write(*,*) "asi: a*t*sen(b*t) eliga 1"
		write(*,*) "Asi: a*sen(b*t)*sen(b*t) eliga 2"
		write(*,*) "asi: a*(t**2)*cos(b*t) eliga 3"
		read(*,*) m
		if(m.eq.1) then
			write(*,*) "Propocione los valores a, b de acuerdo a la forma que eligio con anterioridad"
			read(*,*) e, r
			write(*,*) "propocione los valores e, f tales que R(t)=e*t**f"
			read(*,*) a, q
			DO WHILE (IT.LT.20)
				w=(H/2)
				K1=o(P,e,r,a,IT,q,L)
				K2=o(P+w*K1,e,r,a,IT,q, L)
				K3=o(P+w*K2,e,r,a,IT,q, L)
				K4=o(P+H*K3,e,r,a,IT,q,L)
				P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
				IT=IT+H
				WRITE(4,*)IT, P
				enddo
		else if(m.eq.2) then
			write(*,*) "Propocione los valores a, b de acuerdo a la forma que eligio con anterioridad"
			read(*,*) e, r
			write(*,*) "propocione los valores e, f tales que R(t)=e*t**f"
			read(*,*) a, q
			DO WHILE (IT.LT.20)
				w=(H/2)
				K1=Ko(P,e,r,a,IT,q,L)
				K2=Ko(P+w*K1,e,r,a,IT,q, L)
				K3=Ko(P+w*K2,e,r,a,IT,q, L)
				K4=Ko(P+H*K3,e,r,a,IT,q,L)
				P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
				IT=IT+H
				WRITE(4,*)IT, P
				enddo
		else
			write(*,*) "Propocione los valores a, b de acuerdo a la forma que eligio con anterioridad"
			read(*,*) e, r
			write(*,*) "propocione los valores e, f tales que R(t)=e*t**f"
			read(*,*) a, q
			DO WHILE (IT.LT.20)
				w=(H/2)
				K1=Ki(P,e,r,a,IT,q,L)
				K2=Ki(P+w*K1,e,r,a,IT,q, L)
				K3=Ki(P+w*K2,e,r,a,IT,q, L)
				K4=Ki(P+H*K3,e,r,a,IT,q,L)
				P=P+(W/3)*(K1+(2*K2)+(2*K3)+K4)
				IT=IT+H
				WRITE(4,*)IT, P
				enddo
		endif
	endif 
				
	END PROGRAM PROYECTOCOMPUTO

	double precision FUNCTION F(P,U,R,L)
	DOUBLE PRECISION P ,U, R, L
	F=-((R/L)*P)+(U/L)
	RETURN 
	ENDFUNCTION F

	double precision FUNCTION j(P,U,g,t,n,L)
	DOUBLE PRECISION P ,U, g, t, n, L
	j=-(((g*(t**n))/L)*P)+(U/L)
	RETURN 
	ENDFUNCTION j

	double precision FUNCTION gl(P,a,q,t,R,L)
	DOUBLE PRECISION P ,a,q,t, R, L
	gl=-((R/L)*P)+((a*(sin(q*t)))/L)
	RETURN 
	ENDFUNCTION gl

	double precision FUNCTION gk(P,a,q,t,R,L)
	DOUBLE PRECISION P ,a,q,t, R, L
	gk=-((R/L)*P)+(a*t*(sin(q*t))/L)
	RETURN 
	ENDFUNCTION gk

	double precision FUNCTION gi(P,a,q,t,R,L)
	DOUBLE PRECISION P ,a,q,t, R, L
	gi=-((R/L)*P)+((a*(t**2)*(cos(q*t)))/L)
	RETURN 
	ENDFUNCTION gi

	double precision FUNCTION o(P,e,r,a,t,q,L)
	DOUBLE PRECISION P ,e,r,a,t,q, L
	o=-(((a*(t**q))/L)*p)+(e*t*(sin(r*t))/L)
	RETURN 
	ENDFUNCTION o



	double precision FUNCTION Ki(P,e,r,a,t,q,L)
	DOUBLE PRECISION P ,e,r,a,t,q, L
	Ki=-(((a*(t**q))/L)*p)+(e*(t**2)*(cos(r*t))/L)
	RETURN 
	ENDFUNCTION Ki

	double precision FUNCTION Ko(P,e,r,a,t,q,L)
	DOUBLE PRECISION P ,e,r,a,t,q, L
	Ko=-(((a*(t**q))/L)*p)+(e*((sin(r*t)**2))/L)
	RETURN 
	ENDFUNCTION Ko
