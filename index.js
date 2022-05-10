var Ub=function(t){return function(e){for(var r=e.length,n=new Array(r),a=0;a<r;a++)n[a]=t(e[a]);return n}};var ei={compose:function(t){return function(e){return function(r){return t(e(r))}}}},xo=function(t){return t.compose};var Z=function(t){return t.identity},et={identity:function(t){return t},Semigroupoid0:function(){return ei}};var Ye=!0;var Tt=function(t){return function(e){return function(r){return t(r)(e)}}},T=function(t){return function(e){return t}};var jf=function(t){return function(e){return e(t)}},nl=function(t){return function(e){return t(e)}};var d=function(){function t(){}return t.value=new t,t}();var p=function(t){return t.map},Ze=function(t){return function(e){return function(r){return p(t)(r)(e)}}},Kt=function(t){return p(t)(T(void 0))},X=function(t){return function(e){return function(r){return p(t)(T(r))(e)}}},z_=function(t){return function(e){return p(t)(T(e))}};var Ra={map:xo(ei)},xe={map:Ub},em=function(t){return function(e){return function(r){return p(t)(function(n){return n(r)})(e)}}};var qb=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var Sr=function(t){return t.reflectSymbol};var V_=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},ja=function(t){return function(e){return e[t]}},Xu=function(t){return function(e){return function(r){var n={};for(var a in r)({}).hasOwnProperty.call(r,a)&&(n[a]=r[a]);return n[t]=e,n}}};var zb={append:function(t){return function(e){return void 0}}};var ya={append:qb};var dt=function(t){return t.append},nm=function(t){return{append:function(e){return function(r){return function(n){return dt(t)(e(n))(r(n))}}}}};var I=function(t){return t.alt};var Vb=function(t){return function(e){for(var r=t.length,n=e.length,a=new Array(r*n),u=0,f=0;f<r;f++)for(var i=t[f],m=0;m<n;m++)a[u++]=i(e[m]);return a}};var al={apply:Vb,Functor0:function(){return xe}},Ht=function(t){return t.apply};var j=function(t){return function(e){return function(r){return Ht(t)(p(t.Functor0())(T(Z(et)))(e))(r)}}},ca=function(t){return function(e){return function(r){return function(n){return Ht(t)(p(t.Functor0())(e)(r))(n)}}}};var h=function(t){return t.pure};var $n=function(t){return function(e){return function(r){if(e)return r;if(!e)return h(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},ul=function(t){return function(e){return function(r){return Ht(t.Apply0())(h(t)(e))(r)}}};var cr={pure:function(t){return[t]},Apply0:function(){return al}};var Jb=function(t){return function(e){for(var r=[],n=0,a=t.length;n<a;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var Wr=function(t){return t.discard};var ri={bind:Jb,Apply0:function(){return al}},q=function(t){return t.bind},Yn=function(t){return Tt(q(t))};var Xf=function(t){return function(e){return function(r){return function(n){return q(t)(e(n))(r)}}}};var Er={discard:function(t){return q(t)}};var Zn=function(t){return function(e){return q(t)(e)(Z(et))}};var ft=function(t){return t};var jb=ft;var Xb=function(t){return function(e){return function(){return t(e())}}};function Gr(t){return function(){return{value:t}}}var $r=function(t){return function(){return t.value}},Qb=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},an=function(t){return function(e){return function(){return e.value=t}}};var Qu=function(t){return function(e){return function(r){return q(t.Bind1())(e)(function(n){return q(t.Bind1())(r)(function(a){return h(t.Applicative0())(n(a))})})}}};var T0=String.fromCharCode(65535),x0=String.fromCharCode(0),F0=Number.POSITIVE_INFINITY,$0=Number.NEGATIVE_INFINITY;var Kb=function(t){return function(e){return function(r){return function(n){return function(a){return n<a?t:n===a?e:r}}}}};var Yb=Kb,Zb=Kb;var tA=function(t){return function(e){return t===e}};var eA=tA,rA=tA;var ol={eq:rA},Pi={eq:eA};var le=function(t){return t.eq};var ne=function(){function t(){}return t.value=new t,t}(),be=function(){function t(){}return t.value=new t,t}(),Ae=function(){function t(){}return t.value=new t,t}();var nA=function(t){return function(e){return t-e|0}},aA=function(t){return function(e){return t-e}};var uA=function(t){return function(e){return t+e|0}},oA=function(t){return function(e){return t*e|0}},iA=function(t){return function(e){return t+e}},fA=function(t){return function(e){return t*e}};var ka=function(t){return t.zero};var ga={add:iA,zero:0,mul:fA,one:1},$u={add:uA,zero:0,mul:oA,one:1};var Ca=function(t){return t.one};var Wn=function(t){return t.mul};var Le=function(t){return t.add};var Mu=function(t){return t.sub};var _f={sub:aA,Semiring0:function(){return ga}},om={sub:nA,Semiring0:function(){return $u}};var il=function(t){return function(e){return Mu(t)(ka(t.Semiring0()))(e)}};var Na=function(){return{compare:Zb(ne.value)(Ae.value)(be.value),Eq0:function(){return ol}}}(),Jr=function(){return{compare:Yb(ne.value)(Ae.value)(be.value),Eq0:function(){return Pi}}}();var ee=function(t){return t.compare};var cA=function(t){return function(e){return function(r){var n=ee(t)(e)(r);return!(n instanceof ne)}}};var wu=function(t){return function(e){return function(r){var n=ee(t)(e)(r);if(n instanceof ne)return r;if(n instanceof Ae||n instanceof be)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var cm=function(t){return function(e){return function(r){var n=cA(t)(r)(ka(e.Semiring0()));return n?r:il(e)(r)}}};var Gn=function(t){return t.top};var sf={top:2147483647,bottom:-2147483648,Ord0:function(){return Jr}};var Bn=function(t){return t.bottom};var _A=function(t){return t.toString()},pA=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var j_={show:pA},Xa={show:_A};var jt=function(t){return t.show};var V=function(){function t(){}return t.value=new t,t}(),B=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var Br=function(t){return function(e){return function(r){if(r instanceof V)return t;if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var Mr={map:function(t){return function(e){return e instanceof B?new B(t(e.value0)):V.value}}};var Sa=function(t){return Br(t)(Z(et))},ta=function(){return function(t){if(t instanceof B)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ri={apply:function(t){return function(e){if(t instanceof B)return p(Mr)(t.value0)(e);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return Mr}},Ea={bind:function(t){return function(e){if(t instanceof B)return e(t.value0);if(t instanceof V)return V.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return Ri}};var $o=function(){return{pure:B.create,Apply0:function(){return Ri}}}();var Yt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Zt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var vf={map:function(t){return function(e){if(e instanceof Yt)return new Yt(e.value0);if(e instanceof Zt)return new Zt(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var La=function(t){return function(e){return function(r){if(r instanceof Yt)return t(r.value0);if(r instanceof Zt)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},X_=function(){return La(T(V.value))(B.create)}();var fu=function(t){return t};var Mo={map:function(t){return function(e){return t(e)}}};var sA={apply:function(t){return function(e){return t(e)}},Functor0:function(){return Mo}},j0={bind:function(t){return function(e){return e(t)}},Apply0:function(){return sA}},pm={pure:fu,Apply0:function(){return sA}},Yu={Applicative0:function(){return pm},Bind1:function(){return j0}};var mA=function(t){return Math.min(Math.abs(t),2147483647)},vA=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},DA=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},dA=function(t){return function(e){return t/e}};var bA={Ring0:function(){return _f}},AA={Ring0:function(){return om}};var cu=function(t){return t.mod};var pl={degree:function(t){return 1},div:dA,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return bA}},wo={degree:mA,div:vA,mod:DA,CommutativeRing0:function(){return AA}},Zu=function(t){return t.div};var un={mempty:void 0,Semigroup0:function(){return zb}};var Ot=function(t){return t.mempty},jr=function(t){return{mempty:function(e){return Ot(t)},Semigroup0:function(){return nm(t.Semigroup0())}}};var sm=function(t){return function(){return t}},yA=function(t){return function(e){return function(){return e(t())()}}};var sl=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var kA=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},ue={Applicative0:function(){return c},Bind1:function(){return hr}},hr={bind:yA,Apply0:function(){return mm(0)}},c={pure:sm,Apply0:function(){return mm(0)}},gA=kA("functorEffect","Effect",function(){return{map:ul(c)}}),mm=kA("applyEffect","Effect",function(){return{apply:Qu(ue),Functor0:function(){return gA(0)}}}),x=gA(20),tt=mm(23),CA=function(t){return{append:ca(tt)(dt(t))}},Xr=function(t){return{mempty:sm(Ot(t)),Semigroup0:function(){return CA(t.Semigroup0())}}};var SA=function(t){return function(){return{value:t}}};var wr=function(t){return function(){return t.value}},EA=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},Mn=function(t){return function(e){return function(){e.value=t}}};var lr=SA,tT=EA,Kf=function(t){return tT(function(e){var r=t(e);return{state:r,value:r}})},Df=function(t){return function(e){return Kt(x)(Kf(t)(e))}};var uT=Qb,ha=function(t){return uT(function(e){var r=t(e);return{state:r,value:r}})},Ni={map:Xb};var o={liftST:jb,Monad0:function(){return ue}},It=function(t){return t.liftST};var dn=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),a=t,u=0;a!==e;)n[u++]=a,a+=r;return n[u]=a,n}},iT=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},fT=function(t){return function(e){for(var r=[],n=0,a=0;a<t;a++)r[n++]=e;return r}},Q_=typeof Array.prototype.fill=="function"?iT:fT,cT=function(){function t(a,u){this.head=a,this.tail=u}var e={};function r(a){return function(u){return new t(a,u)}}function n(a){for(var u=[],f=0,i=a;i!==e;)u[f++]=i.head,i=i.tail;return u}return function(a){return function(u){return n(a(r)(e)(u))}}}(),Ou=function(t){return t.length};var TA=function(t){return function(e){return function(r){return function(n){for(var a=0,u=n.length;a<u;a++)if(r(n[a]))return t(a);return e}}}};var xA=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var a=n.slice();return a.splice(r,1),t(a)}}}};var lT=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){var a;return n.length<2?n:(a=n.slice(0),t(e,r,a,n.slice(0),0,n.length),a)}}}}();var ml=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,a=new Array(n),u=0;u<n;u++)a[u]=t(e[u])(r[u]);return a}}};var FA=function(t){return function(e){return t[e]}};var pT=function(){function t(e,r,n,a,u,f){var i,m,s,_,v,D,l;for(i=u+(f-u>>1),i-u>1&&t(e,r,a,n,u,i),f-i>1&&t(e,r,a,n,i,f),m=u,s=i,_=u;m<i&&s<f;)v=a[m],D=a[s],l=r(e(v)(D)),l>0?(n[_++]=D,++s):(n[_++]=v,++m);for(;m<i;)n[_++]=a[m++];for(;s<f;)n[_++]=a[s++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var PA=function(t){return function(e){return t&&e}},IA=function(t){return function(e){return t||e}},RA=function(t){return!t};var pu=function(t){return t.not};var Zf=function(t){return t.disj},Ga={ff:!1,tt:!0,implies:function(t){return function(e){return Zf(Ga)(pu(Ga)(t))(e)}},conj:PA,disj:IA,not:RA};var LA=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=a-1;u>=0;u--)n=t(r[u])(n);return n}}},WA=function(t){return function(e){return function(r){for(var n=e,a=r.length,u=0;u<a;u++)n=t(n)(r[u]);return n}}};var $=function(t){return t.empty};var nt=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),tc=function(t){return function(e){return t(e.value0)(e.value1)}};var on=function(t){return t.value1};var ni={map:function(t){return function(e){return new nt(e.value0,t(e.value1))}}};var Ua=function(t){return t.value0};var Pu=function(){return ft};var fn=Pu,wn=Pu;var Cm=function(){return function(){return function(t){return Pu()}}};var er=function(t){return t.foldr};var dr=function(t){return function(e){return er(t)(I(e.Alt0()))($(e))}},Qr=function(t){return function(e){return function(r){return er(t)(function(){var n=I(e.Alt0());return function(a){return n(r(a))}}())($(e))}}},rr=function(t){return function(e){return function(r){return er(e)(function(){var n=j(t.Apply0());return function(a){return n(r(a))}}())(h(t)(void 0))}}},ea=function(t){return function(e){return Tt(rr(t)(e))}},ap=function(t){return function(e){return rr(t)(e)(Z(et))}},nr=function(t){return t.foldl};var Je={foldr:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof V)return e;if(r instanceof B)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof V)return Ot(t);if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var HA=function(t){return function(e){return function(r){return er(t)(function(n){return function(a){return dt(e.Semigroup0())(r(n))(a)}})(Ot(e))}}},Xt={foldr:LA,foldl:WA,foldMap:function(t){return HA(Xt)(t)}},up=function(t){return function(e){return function(r){return nr(t)(function(n){return function(a){return dt(e.Semigroup0())(n)(r(a))}})(Ot(e))}}},qn=function(t){return t.foldMap};var zA=function(){function t(a){return[a]}function e(a){return function(u){return[a,u]}}function r(a){return function(u){return function(f){return[a,u,f]}}}function n(a){return function(u){return a.concat(u)}}return function(a){return function(u){return function(f){return function(i){return function(m){function s(_,v){switch(v-_){case 0:return f([]);case 1:return u(t)(i(m[_]));case 2:return a(u(e)(i(m[_])))(i(m[_+1]));case 3:return a(a(u(r)(i(m[_])))(i(m[_+1])))(i(m[_+2]));default:var D=_+Math.floor((v-_)/4)*2;return a(u(n)(s(_,D)))(s(D,v))}}return s(0,m.length)}}}}}}();var ra=function(t){return t.traverse};var ty=function(t){return function(e){return ra(t)(e)(Z(et))}},ai={traverse:function(t){return zA(Ht(t.Apply0()))(p(t.Apply0().Functor0()))(h(t))},sequence:function(t){return ty(ai)(t)},Functor0:function(){return xe},Foldable1:function(){return Xt}};var Sl=function(){return ml(nt.create)}();var Rm=function(){return FA};var iy=function(t){return[t]};var fy=function(){return TA(B.create)(V.value)}();var Nm=function(){return xA(B.create)(V.value)}(),Lm=function(t){return function(e){return function(r){return r.length===0?[]:Br(r)(function(n){return ta()(Nm(n)(r))})(fy(t(e))(r))}}};var Gi=function(t){return function(e){return dt(ya)([t])(e)}};var cy=function(t){return function(e){for(var r=e.length,n=Array(r),a=0;a<r;a++)n[a]=t(a)(e[a]);return n}};var ao=function(t){return t.mapWithIndex};var oi={mapWithIndex:cy,Functor0:function(){return xe}};var Po=function(t){return t.foldrWithIndex};var oo=function(t){return t.foldlWithIndex};var ii=function(t){return t.foldMapWithIndex};var Bi=function(t){return t.traverseWithIndex};var io=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var mp=function(t){return function(e){return new io(e,$(t))}};var Pr=function(){function t(){}return t.value=new t,t}(),se=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),vp=function(t){return t},Ux=function(t){return new se(t.value0,t.value1)};var qx=function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof se&&s.value1 instanceof se&&s.value1.value1 instanceof se){a=new se(s,m),n=s.value1.value1.value1;return}var _=function(D){return D instanceof se&&D.value1 instanceof se&&D.value1.value1 instanceof Pr?new se(t(D.value0),new se(t(D.value1.value0),Pr.value)):D instanceof se&&D.value1 instanceof Pr?new se(t(D.value0),Pr.value):Pr.value},v=function(D){return function(l){var g=D,ot=!1,vt;function Wt(re,he){if(re instanceof se&&re.value0 instanceof se&&re.value0.value1 instanceof se&&re.value0.value1.value1 instanceof se){g=re.value1,l=new se(t(re.value0.value0),new se(t(re.value0.value1.value0),new se(t(re.value0.value1.value1.value0),he)));return}return ot=!0,he}for(;!ot;)vt=Wt(g,l);return vt}};return u=!0,v(m)(_(s))}for(;!u;)f=i(a,n);return f}};return e(Pr.value)},Dp={map:qx};var qa={foldr:function(t){return function(e){var r=function(){var a=function(u){return function(f){var i=u,m=!1,s;function _(v,D){if(D instanceof Pr)return m=!0,v;if(D instanceof se){i=new se(D.value0,v),f=D.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[v.constructor.name,D.constructor.name])}for(;!m;)s=_(i,f);return s}};return a(Pr.value)}(),n=nr(qa)(Tt(t))(e);return function(a){return n(r(a))}}},foldl:function(t){var e=function(r){return function(n){var a=r,u=!1,f;function i(m,s){if(s instanceof Pr)return u=!0,m;if(s instanceof se){a=t(m)(s.value0),n=s.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[s.constructor.name])}for(;!u;)f=i(a,n);return f}};return e},foldMap:function(t){return function(e){return nr(qa)(function(r){var n=dt(t.Semigroup0())(r);return function(a){return n(e(a))}})(Ot(t))}}};var El={append:function(t){return function(e){return er(qa)(se.create)(e)(t)}}};var Gm={append:function(t){return function(e){return new io(t.value0,dt(El)(t.value1)(Ux(e)))}}};var sy={alt:dt(El),Functor0:function(){return Dp}},Bm=function(){return{empty:Pr.value,Alt0:function(){return sy}}}();var Ay=function(t){return t()};var yy=function(t){throw new Error(t)};var ky=function(){return yy};var _F=Ay,mu=function(t){return _F(function(){return ky()(t)})};var Qt=function(){function t(){}return t.value=new t,t}(),ve=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}(),We=function(){function t(e,r,n,a,u,f,i){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f,this.value6=i}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return new t(e,r,n,a,u,f,i)}}}}}}},t}(),Ui=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),_i=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),qi=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),Ro=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),Hi=function(){function t(e,r,n,a,u,f){this.value0=e,this.value1=r,this.value2=n,this.value3=a,this.value4=u,this.value5=f}return t.create=function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return new t(e,r,n,a,u,f)}}}}}},t}(),bp=function(){function t(e,r,n,a){this.value0=e,this.value1=r,this.value2=n,this.value3=a}return t.create=function(e){return function(r){return function(n){return function(a){return new t(e,r,n,a)}}}},t}();var Cy=function(t){return function(e){return new ve(Qt.value,t,e,Qt.value)}};var dF=function(t){return function(e){var r=ee(t),n=function(a){var u=!1,f;function i(m){if(m instanceof Qt)return u=!0,V.value;if(m instanceof ve){var s=r(e)(m.value1);if(s instanceof Ae)return u=!0,new B(m.value2);if(s instanceof ne){a=m.value0;return}a=m.value3;return}if(m instanceof We){var _=r(e)(m.value1);if(_ instanceof Ae)return u=!0,new B(m.value2);var v=r(e)(m.value4);if(v instanceof Ae)return u=!0,new B(m.value5);if(_ instanceof ne){a=m.value0;return}if(v instanceof be){a=m.value6;return}a=m.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[m.constructor.name])}for(;!u;)f=i(a);return f};return n}};var Sy=function(t){return t instanceof Qt};var cn=function(t){return function(e){return function(r){var n=t,a=e,u=!1,f;function i(m,s,_){if(s instanceof Pr)return u=!0,_;if(s instanceof se){if(s.value0 instanceof Ui){n=m,a=s.value1,r=new ve(_,s.value0.value0,s.value0.value1,s.value0.value2);return}if(s.value0 instanceof _i){n=m,a=s.value1,r=new ve(s.value0.value0,s.value0.value1,s.value0.value2,_);return}if(s.value0 instanceof qi){n=m,a=s.value1,r=new We(_,s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Ro){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,_,s.value0.value3,s.value0.value4,s.value0.value5);return}if(s.value0 instanceof Hi){n=m,a=s.value1,r=new We(s.value0.value0,s.value0.value1,s.value0.value2,s.value0.value3,s.value0.value4,s.value0.value5,_);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[s.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[s.constructor.name,_.constructor.name])}for(;!u;)f=i(n,a,r);return f}}},xl=function(t){return function(e){return function(r){var n=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(D instanceof Pr)return s=!0,new ve(l.value0,l.value1,l.value2,l.value3);if(D instanceof se){if(D.value0 instanceof Ui)return s=!0,cn(t)(D.value1)(new We(l.value0,l.value1,l.value2,l.value3,D.value0.value0,D.value0.value1,D.value0.value2));if(D.value0 instanceof _i)return s=!0,cn(t)(D.value1)(new We(D.value0.value0,D.value0.value1,D.value0.value2,l.value0,l.value1,l.value2,l.value3));if(D.value0 instanceof qi){m=D.value1,i=new bp(new ve(l.value0,l.value1,l.value2,l.value3),D.value0.value0,D.value0.value1,new ve(D.value0.value2,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Ro){m=D.value1,i=new bp(new ve(D.value0.value0,D.value0.value1,D.value0.value2,l.value0),l.value1,l.value2,new ve(l.value3,D.value0.value3,D.value0.value4,D.value0.value5));return}if(D.value0 instanceof Hi){m=D.value1,i=new bp(new ve(D.value0.value0,D.value0.value1,D.value0.value2,D.value0.value3),D.value0.value4,D.value0.value5,new ve(l.value0,l.value1,l.value2,l.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[D.value0.constructor.name,l.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}},a=ee(t),u=function(f){return function(i){var m=f,s=!1,_;function v(D,l){if(l instanceof Qt)return s=!0,n(D)(new bp(Qt.value,e,r,Qt.value));if(l instanceof ve){var g=a(e)(l.value1);if(g instanceof Ae)return s=!0,cn(t)(D)(new ve(l.value0,e,r,l.value3));if(g instanceof ne){m=new se(new Ui(l.value1,l.value2,l.value3),D),i=l.value0;return}m=new se(new _i(l.value0,l.value1,l.value2),D),i=l.value3;return}if(l instanceof We){var ot=a(e)(l.value1);if(ot instanceof Ae)return s=!0,cn(t)(D)(new We(l.value0,e,r,l.value3,l.value4,l.value5,l.value6));var vt=a(e)(l.value4);if(vt instanceof Ae)return s=!0,cn(t)(D)(new We(l.value0,l.value1,l.value2,l.value3,e,r,l.value6));if(ot instanceof ne){m=new se(new qi(l.value1,l.value2,l.value3,l.value4,l.value5,l.value6),D),i=l.value0;return}if(ot instanceof be&&vt instanceof ne){m=new se(new Ro(l.value0,l.value1,l.value2,l.value4,l.value5,l.value6),D),i=l.value3;return}m=new se(new Hi(l.value0,l.value1,l.value2,l.value3,l.value4,l.value5),D),i=l.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[D.constructor.name,l.constructor.name])}for(;!s;)_=v(m,i);return _}};return u(Pr.value)}}},bF=function(t){return function(e){var r=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(l instanceof Pr)return _=!0,g;if(l instanceof se){if(l.value0 instanceof Ui&&l.value0.value2 instanceof Qt&&g instanceof Qt)return _=!0,cn(t)(l.value1)(new ve(Qt.value,l.value0.value0,l.value0.value1,Qt.value));if(l.value0 instanceof _i&&l.value0.value0 instanceof Qt&&g instanceof Qt)return _=!0,cn(t)(l.value1)(new ve(Qt.value,l.value0.value1,l.value0.value2,Qt.value));if(l.value0 instanceof Ui&&l.value0.value2 instanceof ve){s=l.value1,m=new We(g,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3);return}if(l.value0 instanceof _i&&l.value0.value0 instanceof ve){s=l.value1,m=new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,g);return}return l.value0 instanceof Ui&&l.value0.value2 instanceof We?(_=!0,cn(t)(l.value1)(new ve(new ve(g,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new ve(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6)))):l.value0 instanceof _i&&l.value0.value0 instanceof We?(_=!0,cn(t)(l.value1)(new ve(new ve(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new ve(l.value0.value0.value6,l.value0.value1,l.value0.value2,g)))):l.value0 instanceof qi&&l.value0.value2 instanceof Qt&&l.value0.value5 instanceof Qt&&g instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value0,l.value0.value1,Qt.value,l.value0.value3,l.value0.value4,Qt.value))):l.value0 instanceof Ro&&l.value0.value0 instanceof Qt&&l.value0.value5 instanceof Qt&&g instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value1,l.value0.value2,Qt.value,l.value0.value3,l.value0.value4,Qt.value))):l.value0 instanceof Hi&&l.value0.value0 instanceof Qt&&l.value0.value3 instanceof Qt&&g instanceof Qt?(_=!0,cn(t)(l.value1)(new We(Qt.value,l.value0.value1,l.value0.value2,Qt.value,l.value0.value4,l.value0.value5,Qt.value))):l.value0 instanceof qi&&l.value0.value2 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(new We(g,l.value0.value0,l.value0.value1,l.value0.value2.value0,l.value0.value2.value1,l.value0.value2.value2,l.value0.value2.value3),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Ro&&l.value0.value0 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(new We(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3,l.value0.value1,l.value0.value2,g),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Ro&&l.value0.value5 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(l.value0.value0,l.value0.value1,l.value0.value2,new We(g,l.value0.value3,l.value0.value4,l.value0.value5.value0,l.value0.value5.value1,l.value0.value5.value2,l.value0.value5.value3)))):l.value0 instanceof Hi&&l.value0.value3 instanceof ve?(_=!0,cn(t)(l.value1)(new ve(l.value0.value0,l.value0.value1,l.value0.value2,new We(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3,l.value0.value4,l.value0.value5,g)))):l.value0 instanceof qi&&l.value0.value2 instanceof We?(_=!0,cn(t)(l.value1)(new We(new ve(g,l.value0.value0,l.value0.value1,l.value0.value2.value0),l.value0.value2.value1,l.value0.value2.value2,new ve(l.value0.value2.value3,l.value0.value2.value4,l.value0.value2.value5,l.value0.value2.value6),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Ro&&l.value0.value0 instanceof We?(_=!0,cn(t)(l.value1)(new We(new ve(l.value0.value0.value0,l.value0.value0.value1,l.value0.value0.value2,l.value0.value0.value3),l.value0.value0.value4,l.value0.value0.value5,new ve(l.value0.value0.value6,l.value0.value1,l.value0.value2,g),l.value0.value3,l.value0.value4,l.value0.value5))):l.value0 instanceof Ro&&l.value0.value5 instanceof We?(_=!0,cn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new ve(g,l.value0.value3,l.value0.value4,l.value0.value5.value0),l.value0.value5.value1,l.value0.value5.value2,new ve(l.value0.value5.value3,l.value0.value5.value4,l.value0.value5.value5,l.value0.value5.value6)))):l.value0 instanceof Hi&&l.value0.value3 instanceof We?(_=!0,cn(t)(l.value1)(new We(l.value0.value0,l.value0.value1,l.value0.value2,new ve(l.value0.value3.value0,l.value0.value3.value1,l.value0.value3.value2,l.value0.value3.value3),l.value0.value3.value4,l.value0.value3.value5,new ve(l.value0.value3.value6,l.value0.value4,l.value0.value5,g)))):(_=!0,mu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[l.constructor.name])}for(;!_;)v=D(s,m);return v}},n=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(g instanceof ve&&g.value0 instanceof Qt&&g.value3 instanceof Qt)return _=!0,r(l)(Qt.value);if(g instanceof ve){s=new se(new _i(g.value0,g.value1,g.value2),l),m=g.value3;return}if(g instanceof We&&g.value0 instanceof Qt&&g.value3 instanceof Qt&&g.value6 instanceof Qt)return _=!0,r(new se(new _i(Qt.value,g.value1,g.value2),l))(Qt.value);if(g instanceof We){s=new se(new Hi(g.value0,g.value1,g.value2,g.value3,g.value4,g.value5),l),m=g.value6;return}return _=!0,mu("The impossible happened in partial function `removeMaxNode`.")}for(;!_;)v=D(s,m);return v}},a=function(i){var m=!1,s;function _(v){if(v instanceof ve&&v.value3 instanceof Qt)return m=!0,{key:v.value1,value:v.value2};if(v instanceof ve){i=v.value3;return}if(v instanceof We&&v.value6 instanceof Qt)return m=!0,{key:v.value4,value:v.value5};if(v instanceof We){i=v.value6;return}return m=!0,mu("The impossible happened in partial function `maxNode`.")}for(;!m;)s=_(i);return s},u=ee(t),f=function(i){return function(m){var s=i,_=!1,v;function D(l,g){if(g instanceof Qt)return _=!0,V.value;if(g instanceof ve){var ot=u(e)(g.value1);if(g.value3 instanceof Qt&&ot instanceof Ae)return _=!0,new B(new nt(g.value2,r(l)(Qt.value)));if(ot instanceof Ae){var vt=a(g.value0);return _=!0,new B(new nt(g.value2,n(new se(new Ui(vt.key,vt.value,g.value3),l))(g.value0)))}if(ot instanceof ne){s=new se(new Ui(g.value1,g.value2,g.value3),l),m=g.value0;return}s=new se(new _i(g.value0,g.value1,g.value2),l),m=g.value3;return}if(g instanceof We){var Wt=function(){return g.value0 instanceof Qt&&g.value3 instanceof Qt&&g.value6 instanceof Qt}(),ot=u(e)(g.value4),re=u(e)(g.value1);if(Wt&&re instanceof Ae)return _=!0,new B(new nt(g.value2,cn(t)(l)(new ve(Qt.value,g.value4,g.value5,Qt.value))));if(Wt&&ot instanceof Ae)return _=!0,new B(new nt(g.value5,cn(t)(l)(new ve(Qt.value,g.value1,g.value2,Qt.value))));if(re instanceof Ae){var vt=a(g.value0);return _=!0,new B(new nt(g.value2,n(new se(new qi(vt.key,vt.value,g.value3,g.value4,g.value5,g.value6),l))(g.value0)))}if(ot instanceof Ae){var vt=a(g.value3);return _=!0,new B(new nt(g.value5,n(new se(new Ro(g.value0,g.value1,g.value2,vt.key,vt.value,g.value6),l))(g.value3)))}if(re instanceof ne){s=new se(new qi(g.value1,g.value2,g.value3,g.value4,g.value5,g.value6),l),m=g.value0;return}if(re instanceof be&&ot instanceof ne){s=new se(new Ro(g.value0,g.value1,g.value2,g.value4,g.value5,g.value6),l),m=g.value3;return}s=new se(new Hi(g.value0,g.value1,g.value2,g.value3,g.value4,g.value5),l),m=g.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[g.constructor.name])}for(;!_;)v=D(s,m);return v}};return f(Pr.value)}},Ta={foldr:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return er(Ta)(t)(t(r.value2)(er(Ta)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return er(Ta)(t)(t(r.value2)(er(Ta)(t)(t(r.value5)(er(Ta)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return nr(Ta)(t)(t(nr(Ta)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return nr(Ta)(t)(t(nr(Ta)(t)(t(nr(Ta)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Qt)return Ot(t);if(r instanceof ve)return dt(t.Semigroup0())(qn(Ta)(t)(e)(r.value0))(dt(t.Semigroup0())(e(r.value2))(qn(Ta)(t)(e)(r.value3)));if(r instanceof We)return dt(t.Semigroup0())(qn(Ta)(t)(e)(r.value0))(dt(t.Semigroup0())(e(r.value2))(dt(t.Semigroup0())(qn(Ta)(t)(e)(r.value3))(dt(t.Semigroup0())(e(r.value5))(qn(Ta)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},_a={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return Po(_a)(t)(t(r.value1)(r.value2)(Po(_a)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return Po(_a)(t)(t(r.value1)(r.value2)(Po(_a)(t)(t(r.value4)(r.value5)(Po(_a)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return e;if(r instanceof ve)return oo(_a)(t)(t(r.value1)(oo(_a)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return oo(_a)(t)(t(r.value4)(oo(_a)(t)(t(r.value1)(oo(_a)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Qt)return Ot(t);if(r instanceof ve)return dt(t.Semigroup0())(ii(_a)(t)(e)(r.value0))(dt(t.Semigroup0())(e(r.value1)(r.value2))(ii(_a)(t)(e)(r.value3)));if(r instanceof We)return dt(t.Semigroup0())(ii(_a)(t)(e)(r.value0))(dt(t.Semigroup0())(e(r.value1)(r.value2))(dt(t.Semigroup0())(ii(_a)(t)(e)(r.value3))(dt(t.Semigroup0())(e(r.value4)(r.value5))(ii(_a)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return Ta}},Ey=function(){return Po(_a)(function(t){return function(e){return function(r){return new se(t,r)}}})(Pr.value)}();var yp=function(){return Qt.value}();var Vm=function(t){return function(e){return function(r){return Br(r)(on)(bF(t)(e)(r))}}};var kp=function(t){return function(e){return function(r){return function(n){var a=e(dF(t)(r)(n));if(a instanceof V)return Vm(t)(r)(n);if(a instanceof B)return xl(t)(r)(a.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[a.constructor.name])}}}};var AF=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return function(i){return kp(t)(function(){var m=Br(i)(e(i));return function(s){return B.create(m(s))}}())(u)(f)}}};return oo(_a)(a)(n)(r)}}}};var hy=function(t){return AF(t)(T)};var $l=function(t){return t.partitionMap};var zi=function(t){return t.filterMap};var Ml=function(t){return t.filter};var SF=function(t){return t},wl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Ol=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),EF=function(t){return t},gp=Pu(),b=EF;var L=function(){return wl.create}();var ct=function(){return Ol.create}(),Xe=function(){var t=p(Ra)(p(x)(T(!0)));return function(e){return SF(t(e))}}(),K=function(t){return t.attr};var No=function(t){return t.reflectType};var Xm={map:function(t){return function(e){return p(xe)(t)(e)}}};var FF=function(t){return No(t)},kf=function(){return function(t){return t}};var Py=function(t){return[t]};var Iy=function(){return function(){return function(){return function(){return function(){return function(t){return function(e){return function(r){return r[FF(t)(e)]}}}}}}}};var Qm=[];var gf=function(){return function(){return function(t){return function(e){return Gi(t)(e)}}}};var xa={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},Lo=function(t){return t.dimap},fo=function(t){return function(e){return Lo(t)(e)(Z(et))}};var MF=function(t){return function(e){return function(r){return hy(t)(e)(r)}}};var Ym=function(t){return Ey(t)};var Ly=function(t){return Cy(t)(void 0)};var Zm=function(t){return{append:MF(t)}};var Wy=function(t){return Sy(t)},Gy=function(t){return function(e){return function(r){return xl(t)(e)(void 0)(r)}}};var By={foldMap:function(t){return function(e){var r=qn(qa)(t)(e);return function(n){return r(Ym(n))}}},foldl:function(t){return function(e){var r=nr(qa)(t)(e);return function(n){return r(Ym(n))}}},foldr:function(t){return function(e){var r=er(qa)(t)(e);return function(n){return r(Ym(n))}}}};var tv=yp;var Uy=function(t){return{mempty:tv,Semigroup0:function(){return Zm(t)}}};var Cp=function(t){return function(e){return function(r){return Vm(t)(e)(r)}}};function qy(t){return function(e){return function(){return setTimeout(e,t)}}}function Hy(t){return function(){clearTimeout(t)}}var Sp=qy;var OF={eq:function(t){return function(e){return t===e}}},Ep={compare:function(t){return function(e){return ee(Jr)(t)(e)}},Eq0:function(){return OF}};var Il=Hy;var co=function(t){return t.sampleOn};var br=function(t){return t.keepLatest},Iu=function(t){return t.fold};var Rl=function(t){return function(e){return function(r){return function(n){return zi(t.Filterable1())(on)(Iu(t)(function(a){return function(u){return p(ni)(h($o))(e(a)(u.value0))}})(r)(new nt(n,V.value)))}}}},hp=function(t){return function(e){var r=function(n){return function(a){if(a instanceof V)return new B({now:n,last:V.value});if(a instanceof B)return new B({now:n,last:new B(a.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,a.constructor.name])}};return zi(t.Filterable1())(Z(et))(Iu(t)(r)(e)(V.value))}},Nl=function(t){return t.fix};var Pn=function(t){return function(e){return function(r){return I(t.Plus0().Alt0())(co(t)(e)(r))(co(t)(r)(p(t.Filterable1().Functor1())(jf)(e)))}}},Y=function(t){return t.bang};function rv(t){return function(e){return t===e}}var nv=rv;var NF=function(t){return t};var Et=function(t){return function(e){return t(e)}},LF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(It(t)(Gr(V.value)))(function(u){return q(t.Monad0().Bind1())(r(function(f){return It(t)(Kt(Ni)(an(new B(f))(u)))}))(function(f){return q(t.Monad0().Bind1())(n(function(i){return q(t.Monad0().Bind1())(It(t)($r(u)))(rr(e)(Je)(function(m){return a(i(m))}))}))(function(i){return h(e)(j(e.Apply0())(f)(i))})})})}}}}},Ut=NF,WF=function(t){return function(e){return function(r){return q(t.Monad0().Bind1())(It(t)(Gr(V.value)))(function(n){return q(t.Monad0().Bind1())(e(function(a){return Wr(Er)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(It(t)($r(n)))(ap(t.Monad0().Applicative0())(Je)))(function(){return q(t.Monad0().Bind1())(Et(a)(r))(function(u){return It(t)(Kt(Ni)(an(new B(u))(n)))})})}))(function(a){return h(t.Monad0().Applicative0())(Wr(Er)(t.Monad0().Bind1())(q(t.Monad0().Bind1())(It(t)($r(n)))(ap(t.Monad0().Applicative0())(Je)))(function(){return a}))})})}}},k={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}};var GF=function(t){return function(e){return function(r){return function(n){return function(a){return q(t.Monad0().Bind1())(It(t)(Gr(n)))(function(u){return r(function(f){return q(t.Monad0().Bind1())(It(t)(ha(e(f))(u)))(a)})})}}}}},Ll=function(t){return function(e){return function(r){return function(n){return r(function(a){var u=e(a);if(u instanceof B)return n(u.value0);if(u instanceof V)return h(t)(void 0);throw new Error("Failed pattern match at FRP.Event (line 126, column 13 - line 128, column 27): "+[u.constructor.name])})}}}},av=function(t){return function(e){return Ll(t)(function(r){var n=e(r);if(n)return new B(r);if(!n)return V.value;throw new Error("Failed pattern match at FRP.Event (line 84, column 13 - line 86, column 25): "+[n.constructor.name])})}},Ru=function(t){return function(e){return Ut(function(r){return function(){var a=lr(Ot(Uy(Ep)))(),u=Et(e)(function(f){return function(){var m=lr(V.value)(),s=Sp(t)(function(){r(f)();var v=wr(m)();return Br(h(c)(void 0))(function(D){return Df(Cp(Ep)(D))(a)})(v)()})();return Mn(new B(s))(m)(),Df(dt(Zm(Ep))(Ly(s)))(a)()}})();return function(){var i=wr(a)();return ea(c)(By)(i)(Il)(),u()}}})}};var Wl=function(t){return function(e){return q(t.Monad0().Bind1())(It(t)(Gr([])))(function(r){return h(t.Monad0().Applicative0())({event:function(n){return q(e.Monad0().Bind1())(It(e)(ha(function(a){return dt(ya)(a)([n])})(r)))(function(){return h(e.Monad0().Applicative0())(q(e.Monad0().Bind1())(It(e)(ha(Lm(nv)(n))(r)))(function(){return h(e.Monad0().Applicative0())(void 0)}))})},push:function(n){return q(e.Monad0().Bind1())(It(e)($r(r)))(rr(e.Monad0().Applicative0())(Xt)(function(a){return a(n)}))}})})}},BF=function(t){return function(e){return function(r){return function(n){return q(e.Bind1())(Wl(t)(t))(function(a){var u=r(a.event);return q(e.Bind1())(Et(u.input)(a.push))(function(f){return q(e.Bind1())(Et(u.output)(n))(function(i){return h(e.Applicative0())(j(e.Bind1().Apply0())(f)(i))})})})}}}},Vi=function(t){return function(e){return function(r){return Ut(function(n){return q(t.Monad0().Bind1())(Wl(t)(t))(function(a){return Wr(Er)(t.Monad0().Bind1())(n(r(a.event)))(function(){return Et(e)(a.push)})})})}}},Vy=function(t){return{compact:Ll(t)(Z(et)),separate:function(e){return{left:Ll(t)(function(r){if(r instanceof Yt)return new B(r.value0);if(r instanceof Zt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 67, column 13 - line 69, column 33): "+[r.constructor.name])})(e),right:Ll(t)(function(r){if(r instanceof Zt)return new B(r.value0);if(r instanceof Yt)return V.value;throw new Error("Failed pattern match at FRP.Event (line 74, column 13 - line 76, column 32): "+[r.constructor.name])})(e)}}}},Du=function(t){return{filter:av(t),filterMap:Ll(t),partition:function(e){return function(r){return{yes:av(t)(e)(r),no:av(t)(function(){var n=pu(Ga);return function(a){return n(e(a))}}())(r)}}},partitionMap:function(e){return function(r){return{left:zi(Du(t))(function(){var n=La(B.create)(T(V.value));return function(a){return n(e(a))}}())(r),right:zi(Du(t))(function(n){return X_(e(n))})(r)}}},Compactable0:function(){return Vy(t)},Functor1:function(){return k}}},Zr=function(t){return function(e){return Ut(function(r){return q(t.Monad0().Bind1())(Wl(t)(t))(function(n){return Wr(Er)(t.Monad0().Bind1())(r(e(n.push)(n.event)))(function(){return h(t.Monad0().Applicative0())(h(t.Monad0().Applicative0())(void 0))})})})}},Ft=function(t){return function(e){return function(r){return p(t.Apply0().Functor0())(function(n){return h(t)(void 0)})(r(e))}}},P=function(t){return{alt:function(e){return function(r){return function(n){return Ht(t.Apply0())(p(t.Apply0().Functor0())(function(a){return function(u){return j(t.Apply0())(a)(u)}})(e(n)))(r(n))}}},Functor0:function(){return k}}},C=function(t){return{empty:function(e){return h(t)(h(t)(void 0))},Alt0:function(){return P(t)}}},E=function(t){return{fold:GF(t),keepLatest:WF(t),sampleOn:LF(t)(t.Monad0().Applicative0()),fix:BF(t)(t.Monad0()),bang:Ft(t.Monad0().Applicative0()),Plus0:function(){return C(t.Monad0().Applicative0())},Filterable1:function(){return Du(t.Monad0().Applicative0())}}};var Tp="_____$__$_$$_vbus";function uv(t){return t[Tp]=Tp,t}function ov(t){return()=>{for(let e in t)delete t[e]}}function iv(t){return()=>{let e=(u,f,i,m)=>{let s=Object.keys(m);for(var _=0;_<s.length;_++)if(m[s[_]]instanceof Object&&m[s[_]][Tp]===Tp){let v={},D={};e(u,v,D,m[s[_]]),f[s[_]]=v,i[s[_]]=D}else{let v=`${Math.random()}`;u[v]={},f[s[_]]=D=>()=>{let l=Object.keys(u[v]);for(var g=0;g<l.length;g++)u[v][l[g]](D)()},i[s[_]]=D=>()=>{let l=`${Math.random()}`;return u[v][l]=D,()=>{delete u[v][l]}}}},r={},n={},a={};return e(r,n,a,t),{p:n,e:a,s:r}}}function Gl(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var a in t)({}).hasOwnProperty.call(t,a)&&(r[a]=t[a]);return r}var jy=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Xu(Sr(t)(e))(r)(n)}}}}}};var Xy=function(){return function(){return function(t){return function(e){return Gl(t,e)}}}},Bl=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Xu(Sr(t)(e))(r)(n)}}}}}},pi=function(t){return function(){return function(e){return function(r){return ja(Sr(t)(e))(r)}}}};var Hn={vb:function(t){return function(e){return function(r){return{}}}}},xp=function(t){return t.vb},du=function(){return function(t){return function(e){return function(r){return function(n){var a=xp(e)(d.value)(d.value)(d.value);return Ut(function(u){return q(t.Monad0().Bind1())(iv(a))(function(f){return Wr(Er)(t.Monad0().Bind1())(u(n(f.p)(f.e)))(function(){return h(t.Monad0().Applicative0())(ov(f.s))})})})}}}}},Nu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(a){return function(u){return Bl(t)()()(d.value)(uv(xp(e)(d.value)(d.value)(d.value)))(xp(r)(d.value)(d.value)(d.value))}}}}}}}}}}}}}},ar=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(a){return Bl(t)()()(d.value)(void 0)(xp(e)(d.value)(d.value)(d.value))}}}}}}}}}}}};var cv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Qy=function(){function t(){}return t.value=new t,t}(),lv=function(){function t(){}return t.value=new t,t}(),_v=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),H=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),pv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),U=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var rt=function(t){return new pv(t)};function Ky(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Lu={};function sv(t){return t()}function Yy(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function Zy(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function tk(t){return function(e){return function(r){return function(n){var a=r;function u(i){return function(m){return e(m)(i)(n[i])}}for(var f in n)hasOwnProperty.call(n,f)&&(a=t(a)(u(f)));return a}}}}function Ul(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var JF=Object.keys||Ul(function(t){return function(){return t}});function mv(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var vv=function(t){return function(e){return function(){return delete e[t],e}}};var Dv=Ul(function(t){return function(e){return e}});var t$=Ky;var rk=function(t){return function(e){return sv(function(){var n=t$(e)();return t(n)(),n})}};var nk=function(t){return function(e){return Zy(e,t)}};var Wu=function(t){return function(e){return rk(mv(t)(e))}},Fp={map:function(t){return function(e){return Yy(e,t)}}},e$={mapWithIndex:nk,Functor0:function(){return Fp}},dv=function(){return ft};var $p=tk(jf),ak=function(t){return function(e){return $p(function(r){return function(n){return function(a){return dt(t.Semigroup0())(r)(e(n)(a))}}})(Ot(t))}},Sf={foldl:function(t){return $p(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return er(Xt)(t)(e)(Dv(r))}}},foldMap:function(t){return function(e){return ak(t)(T(e))}}},uk={foldlWithIndex:function(t){return $p(Tt(t))},foldrWithIndex:function(t){return function(e){return function(r){return er(Xt)(tc(t))(e)(Ul(nt.create)(r))}}},foldMapWithIndex:function(t){return ak(t)},Foldable0:function(){return Sf}},r$={traverseWithIndex:function(t){return function(e){return function(r){return $p(function(n){return function(a){return function(u){return Ht(t.Apply0())(p(t.Apply0().Functor0())(Tt(Wu(a)))(n))(e(a)(u))}}})(h(t)(Lu))(r)}}},FunctorWithIndex0:function(){return e$},FoldableWithIndex1:function(){return uk},Traversable2:function(){return ic}},ic={traverse:function(t){var e=Bi(r$)(t);return function(r){return e(T(r))}},sequence:function(t){return ra(ic)(t)(Z(et))},Functor0:function(){return Fp},Foldable1:function(){return Sf}};var fc=function(t){return rk(vv(t))};var Bu={proof:function(t){return t},Coercible0:function(){}},bv=function(t){return t.proof};var ok=function(){function t(){}return t.value=new t,t}(),Av=function(){function t(){}return t.value=new t,t}(),n$=function(){function t(){}return t.value=new t,t}();var a$=function(t){return t.makeText},u$=function(t){return function(e){return function(r){return p(k)(function(n){return t.setText(function(a){return{id:e,text:a}}(n))})(r)}}},o$=function(t){return function(e){return function(r){return p(k)(function(n){return function(a){if(a.value instanceof wl)return t.setProp({id:e,key:a.key,value:a.value.value0});if(a.value instanceof Ol)return t.setCb({id:e,key:a.key,value:a.value.value0});throw new Error("Failed pattern match at Deku.Control (line 73, column 26 - line 75, column 45): "+[a.value.constructor.name])}(gp(n))})(r)}}},i$=function(t){return t.makeElement},ln=function(t){return function(e){var r=function(n){return function(a){return Ut(function(u){return q(t.Bind1())(a.ids)(function(f){return Wr(Er)(t.Bind1())(n.raiseId(f))(function(){return p(t.Bind1().Apply0().Functor0())(j(t.Bind1().Apply0())(u(a.deleteFromCache({id:f}))))(Et(dr(Xt)(C(t.Applicative0()))([Ft(t.Applicative0())(a$(a)({id:f,parent:n.parent,scope:n.scope})),u$(a)(f)(e)]))(u))})})})}};return new U(r)}},ur=function(t){return function(e){return ln(t)(Ft(t.Applicative0())(e))}},ik=function(t){return function(e){return function(r){var n=function(a){var u=function(f){return function(i){return new nt(i+1|0,new nt(f,i))}};return Rl(E(t))(u)(a)(0)};return new _v(br(E(t))(Vi(t)(n(r))(function(a){return p(k)(function(u){return I(P(t.Monad0().Applicative0()))(Ft(t.Monad0().Applicative0())(new cv(e(u.value0))))(p(k)(T(lv.value))(Ml(Du(t.Monad0().Applicative0()))(function(){var f=le(Pi)(u.value1+1|0);return function(i){return f(on(i))}}())(a)))})(a)})))}}},f$=function(t){return function(e){return ft}},J=function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return Ut(function(i){return q(t.Monad0().Bind1())(f.ids)(function(m){return Wr(Er)(t.Monad0().Bind1())(u.raiseId(m))(function(){return p(t.Monad0().Bind1().Apply0().Functor0())(j(t.Monad0().Bind1().Apply0())(i(f.deleteFromCache({id:m}))))(Et(I(P(t.Monad0().Applicative0()))(dr(Xt)(C(t.Monad0().Applicative0()))([Ft(t.Monad0().Applicative0())(i$(f)({id:m,parent:u.parent,scope:u.scope,tag:e})),o$(f)(m)(r)]))(Ef(t.Monad0().Applicative0())(t)({parent:m,scope:u.scope,raiseId:function(s){return h(t.Monad0().Applicative0())(void 0)}})(f)(n)))(i))})})})}};return a}}}},Ef=function(t){return function(e){return function(r){return function(n){var a=function(u){return u(r)(n)};return function(u){if(u instanceof H)return Qr(Xt)(C(t))(Ef(t)(e)(r)(n))(u.value0);if(u instanceof pv)return br(E(e))(p(k)(Ef(t)(e)(r)(n))(u.value0));if(u instanceof U)return a(u.value0);if(u instanceof _v)return Ut(function(f){return q(e.Monad0().Bind1())(It(e)(Gr(Lu)))(function(i){return q(e.Monad0().Bind1())(Et(u.value0)(function(m){return q(e.Monad0().Bind1())(n.ids)(function(s){return q(e.Monad0().Bind1())(It(e)(Gr(h(t)(void 0))))(function(_){return q(e.Monad0().Bind1())(n.ids)(function(v){return q(e.Monad0().Bind1())(It(e)(Gr(h(t)(void 0))))(function(D){return q(e.Monad0().Bind1())(It(e)(Gr(V.value)))(function(l){return q(e.Monad0().Bind1())(It(e)(Gr(h(t)(void 0))))(function(g){return q(e.Monad0().Bind1())(n.ids)(function(ot){return q(e.Monad0().Bind1())(It(e)(Gr(ok.value)))(function(vt){return q(e.Monad0().Bind1())(Et(m)(function(Wt){return q(e.Monad0().Bind1())(It(e)($r(vt)))(function(re){return Wt instanceof Qy&&re instanceof Av?q(e.Monad0().Bind1())(It(e)($r(l)))(rr(t)(Je)(function(he){return f(n.sendToTop(function(gr){return{id:gr}}(he)))})):Wt instanceof lv&&re instanceof Av?Wr(Er)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(It(e)(an(n$.value)(vt))))(function(){var he=j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(j(t.Apply0())(q(e.Monad0().Bind1())(It(e)($r(l)))(rr(t)(Je)(function(gr){return f(n.disconnectElement({id:gr,parent:r.parent,scope:ot}))})))(Zn(e.Monad0().Bind1())(It(e)($r(_)))))(Zn(e.Monad0().Bind1())(It(e)($r(D)))))(Kt(t.Apply0().Functor0())(It(e)(ha(fc(s))(i)))))(Kt(t.Apply0().Functor0())(It(e)(ha(fc(v))(i))));return j(t.Apply0())(Kt(t.Apply0().Functor0())(It(e)(an(he)(g))))(he)}):Wt instanceof cv&&re instanceof ok?Wr(Er)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(It(e)(an(Av.value)(vt))))(function(){return q(e.Monad0().Bind1())(Et(Ef(t)(e)({parent:r.parent,scope:ot,raiseId:function(he){return Kt(t.Apply0().Functor0())(It(e)(an(new B(he))(l)))}})(n)(function(){return Wt.value0 instanceof U?Wt.value0:new U(J(e)("div")($(C(t)))(Wt.value0))}()))(f))(function(he){return Wr(Er)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(It(e)(ha(Wu(v)(he))(i))))(function(){return Kt(t.Apply0().Functor0())(It(e)(an(he)(D)))})})}):h(t)(void 0)})}))(function(Wt){return Wr(Er)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(It(e)(an(Wt)(_))))(function(){return Wr(Er)(e.Monad0().Bind1())(Kt(t.Apply0().Functor0())(It(e)(ha(Wu(s)(Wt))(i))))(function(){return Zn(e.Monad0().Bind1())(It(e)($r(g)))})})})})})})})})})})})}))(function(m){return h(t)(Wr(Er)(e.Monad0().Bind1())(q(e.Monad0().Bind1())(It(e)($r(i)))(nr(Sf)(j(t.Apply0()))(h(t)(void 0))))(function(){return m}))})})});throw new Error("Failed pattern match at Deku.Control (line 304, column 61 - line 381, column 20): "+[u.constructor.name])}}}}},c$=function(t){return function(e){return function(r){return function(n){return Ut(function(a){return q(t.Monad0().Bind1())(n.ids)(function(u){return Et(I(P(t.Monad0().Applicative0()))(Ft(t.Monad0().Applicative0())(n.makeRoot({id:u,root:e})))(Ef(t.Monad0().Applicative0())(t)({parent:u,scope:"rootScope",raiseId:function(f){return h(t.Monad0().Applicative0())(void 0)}})(n)(f$(Bu)(Bu)(r))))(a)})})}}}};var fk=function(t){return function(e){return function(r){return c$(t)(e)(new H(r))}}};var _r=function(){function t(){}return t.value=new t,t}();var pr={attr:function(t){return function(e){return b({key:"click",value:ct(e)})}}};var Vt=function(){function t(){}return t.value=new t,t}();var Mp={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var ck={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var pt={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var lk={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}},cc={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var yv={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var _k={attr:function(t){return function(e){return b({key:"style",value:L(e)})}}};var kv=function(t){return function(e){return function(r){return new U(J(t)("a")(e)(new H(r)))}}};var Te=function(t){return function(e){return function(r){return new U(J(t)("div")(e)(new H(r)))}}},Be=function(t){return Te(t)($(C(t.Monad0().Applicative0())))};var _c=function(t){return function(e){return function(r){return new U(J(t)("span")(e)(new H(r)))}}},gv=function(t){return _c(t)($(C(t.Monad0().Applicative0())))};var Cv=(t,e,r)=>{e!=="@portal@"&&r.units[e].main.appendChild(r.units[t].main)},sk=t=>e=>r=>()=>{var n,a=e.id;r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(a),t&&e.parent!=="@portal@"&&(n=document.body.querySelectorAll("[data-deku-ssr-"+a+"]").item(0))?r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:n}:(r.units[a]={listeners:{},parent:e.parent,scope:e.scope,main:document.createElement(e.tag)},Cv(a,e.parent,r))},mk=t=>e=>r=>()=>{var n=e.id,a;r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n),t&&e.parent!=="@portal@"&&(a=document.body.querySelectorAll("[data-deku-ssr-"+e.parent+"]").item(0))?r.units[n]={main:a.childNodes[0],parent:e.parent,scope:e.scope}:(r.units[n]={main:document.createTextNode(""),parent:e.parent,scope:e.scope},Cv(n,e.parent,r))};function Sv(){return{units:{},scopes:{}}}var vk=t=>e=>r=>()=>{var n=e.id,a=e.value;t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),r.units[n].main.tagName==="INPUT"&&e.key==="value"?r.units[n].main.value=a:r.units[n].main.tagName==="INPUT"&&e.key==="checked"?r.units[n].main.checked=a==="true":r.units[n].main.setAttribute(e.key,a)},Dk=t=>e=>r=>()=>{var n=e.id,a=e.value;if(t&&!r.units[n]&&(dom=document.body.querySelectorAll("[data-deku-ssr-"+n+"]").item(0))&&(r.units[n]={listeners:{},parent:e.parent,scope:e.scope,main:dom},r.scopes[e.scope]||(r.scopes[e.scope]=[]),r.scopes[e.scope].push(n)),e.key==="@self@")a(r.units[n].main)();else{r.units[n].listeners[e.key]&&r.units[n].main.removeEventListener(e.key,r.units[n].listeners[e.key]);var u=f=>a(f)();r.units[n].main.addEventListener(e.key,u),r.units[n].listeners[e.key]=u}},dk=t=>e=>()=>{var r=t.id;e.units[r].main.nodeValue=t.text},bk=t=>e=>r=>()=>{var n,a,u=e.id,f=e.html,i=e.verb,m=e.cache,s=e.parent,_=e.scope,v=e.pxScope;if(t&&s!=="@portal@"&&(n=document.body.querySelectorAll("[data-deku-ssr-"+u+"]").item(0)))r.units[u]={listeners:{},scope:_,parent:s,main:n};else{let l=Object.entries(m);for(var D=0;D<l.length;D++){let g=l[D][0];l[D][1]===!0?f=f.replace(i+g+i,'data-deku-attr-internal="'+g+'"'):f=f.replace(i+g+i,'<span style="display:contents;" data-deku-elt-internal="'+g+'"></span>')}a=document.createElement("div"),a.innerHTML=f.trim(),r.units[u]={listeners:{},scope:_,parent:s,main:a.firstChild}}r.scopes[_]||(r.scopes[_]=[]),r.scopes[_].push(u),a||(a=n),a.querySelectorAll("[data-deku-attr-internal]").forEach(function(l){var g=l.getAttribute("data-deku-attr-internal");let ot=g+v;r.units[ot]={listeners:{},main:l,scope:_},r.scopes[_].push(ot)}),a.querySelectorAll("[data-deku-elt-internal]").forEach(function(l){var g=l.getAttribute("data-deku-elt-internal");let ot=g+v;r.units[g+v]={listeners:{},main:l,scope:_},r.scopes[_].push(ot)}),n||Cv(u,s,r)},Ak=t=>e=>()=>{var r=t.id;e.units[r]={main:t.root}},yk=t=>e=>()=>{var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)},kk=t=>e=>()=>{var r=t.id;e.units[r].noop||e.units[r].containingScope&&e.units[r].containingScope!==t.scope||e.units[r].main.remove()},gk=t=>e=>()=>{delete e.units[t.id]},Ck=t=>e=>()=>{var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)};var Sk=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},qe=function(t){return t};var Ev=function(t){return function(e){return Math.pow(t,e)|0}};var wp=isFinite;var Hl=Math.floor;var Xi=function(t){return function(e){return Math.pow(t,e)}},zl=function(t){return function(e){return t%e}},Op=Math.round;var Pp=Math.sin;var Qi=3.141592653589793;var pc=function(){return Sk(B.create)(V.value)}(),hk=function(t){if(!wp(t))return 0;if(t>=qe(Gn(sf)))return Gn(sf);if(t<=qe(Bn(sf)))return Bn(sf);if(Ye)return Sa(0)(pc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},Tk=function(t){return hk(Op(t))};var Vl=function(t){return hk(Hl(t))};var Uu=Math.random;var Jl=function(t){return function(e){return function(){var n=Uu(),a=(qe(e)-qe(t)+1)*n+qe(t);return Vl(a)}}};var xk=function(t){return t};var d$=1,Ip=2147483647,b$=function(){return Ip-1|0}(),mc=function(t){var e=function(r){return function(n){return function(a){var u=n-r|0,f=cu(wo)(a)(u),i=f<r;return i?f+n|0:f}}};return e(d$)(b$)(t)};var A$=0,y$=48271,Fk=function(t){return function(e){return ta()(pc(zl(qe(y$)*qe(e)+qe(t))(qe(Ip))))}},$k=Fk(A$);var x$=function(){function t(f){this.fn=f}var e={},r=function(f,i){this.head=f,this.tail=i};function n(f){return new r(f,e)}function a(f){return function(i){return new r(f,i)}}function u(f){for(var i=[],m=f;m!==e;)i.push(m.head),m=m.tail;return i}return function(f){return function(i){return function(m){var s=function(v,D){return f(i(a)(m(v)))(D)},_=function(v,D,l){if(D===0)return v;var g=l[D-1];return new t(function(){var ot=_(s(g,v),D-1,l);return ot})};return function(v){for(var D=i(n)(m(v[v.length-1])),l=_(D,v.length-1,v);l instanceof t;)l=l.fn();return i(u)(l)}}}}}();var Ik=function(t){return t};var Rk=ya;var Nk=Xt;var Gk=Ik,Xl=function(t){return t};var Ql=function(t){return Gk(iy(t))};var vc=function(t){if(Ou(t)>0)return new B(Gk(t));if(Ye)return V.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var Bk=function(t){return function(e){return t(Xl(e))}};var Uk=Bk(Ou);var qk=function(){return Bk(Rm())};var si=function(t){return t.state};function Go(t){return new Error(t)}function Tf(t){return function(){throw t}}function Lp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var _o=function(t){return t.throwError};var cM={throwError:Tf,Monad0:function(){return ue}};var Nv={catchError:Tt(Lp),MonadThrow0:function(){return cM}};var mi=function(t){return t.catchError};var Yl=function(t){return function(e){return mi(t)(p(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Zt.create)(e))(function(){var r=h(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(Yt.create(n))}}())}};var or={liftEffect:Z(et),Monad0:function(){return ue}},sr=function(t){return t.liftEffect};var Wv=function(t){return{map:function(e){return function(r){return function(n){return p(t)(function(a){return new nt(e(a.value0),a.value1)})(r(n))}}}}};var Gv=function(t){return{Applicative0:function(){return Uv(t)},Bind1:function(){return Bv(t)}}},Bv=function(t){return{bind:function(e){return function(r){return function(n){return q(t.Bind1())(e(n))(function(a){var u=r(a.value0);return u(a.value1)})}}},Apply0:function(){return qp(t)}}},qp=function(t){return{apply:Qu(Gv(t)),Functor0:function(){return Wv(t.Bind1().Apply0().Functor0())}}},Uv=function(t){return{pure:function(e){return function(r){return h(t.Applicative0())(new nt(e,r))}},Apply0:function(){return qp(t)}}};var Kk=function(t){return{state:function(e){var r=h(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return Gv(t)}}};var Zk=function(t){return function(e){var r=t(e);return r.value0}};var dM=function(t){return t};var eg=function(){var t=function(e){return new nt(xk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=$k(e.newSeed),r}())};return si(Kk(Yu))(t)}();var po=Wv(Mo),rg=p(po)(function(t){return qe(t)/qe(Ip)})(eg);var bc=function(t){return Zk(dM(t))};var Ff=Bv(Yu);var $f=qp(Yu),tg=function(t){return function(e){var r=qe(e),n=qe(t),a=function(i){return n+zl(i)(r-n+1)},u=p(po)(qe)(eg),f=Ht($f)(p(po)(Le(ga))(u))(p(po)(Wn(ga)(2))(u));return p(po)(function(i){return Vl(a(i))})(f)}},qv=function(t){return function(e){var r=t<=e;return r?tg(t)(e):tg(e)(t)}};var n_=Uv(Yu);var Hv=function(t){return q(Ff)(qv(0)(Uk(t)-1|0))(function(e){return h(n_)(qk()(t)(e))})};var u_=function(t){return t.arbitrary};var ng={arbitrary:rg};var ag=function(){return{arbitrary:qv(-1e6)(1e6)}}();var ug=function(t){return{ids:function(){var r=wr(t)(),n=jt(Xa)(bc(u_(ag))({newSeed:mc(r),size:5}));return Kt(x)(Kf(Le($u)(1))(t))(),n},makeElement:sk(!1),makeRoot:Ak,makeText:mk(!1),makePursx:bk(!1),setProp:vk(!1),setCb:Dk(!1),setText:dk,sendToTop:Ck,deleteFromCache:gk,giveNewParent:yk,disconnectElement:kk}};var CM=function(t){return t};var Q=function(t){return{pursxToElement:function(e){return function(r){return function(n){return{cache:Lu,element:function(a){return function(u){return $(C(t))}}}}}}}},Vv=function(t){return t.pursxToElement},_n=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=Vv(t)(a)(d.value)(f);return{cache:Wu(No(e)(d.value))(!0)(i.cache),element:function(m){return function(s){return I(P(n.Monad0().Applicative0()))(p(k)(fo(xa)(gp)(function(_){if(_.value instanceof wl)return s.setProp({id:No(e)(d.value)+a,key:_.key,value:_.value.value0});if(_.value instanceof Ol)return s.setCb({id:No(e)(d.value)+a,key:_.key,value:_.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 4472, column 38 - line 4482, column 24): "+[_.value.constructor.name])}))(pi(r)()(d.value)(f)))(i.element(m)(s))}}}}}}}}}}}},R=function(){return function(t){return function(e){return function(r){return function(n){return{pursxToElement:function(a){return function(u){return function(f){var i=pi(r)()(d.value)(f),m=Vv(t)(a)(d.value)(f);return{cache:Wu(No(e)(d.value))(!1)(m.cache),element:function(s){return function(_){return I(P(n.Monad0().Applicative0()))(Ef(n.Monad0().Applicative0())(n)({parent:No(e)(d.value)+a,scope:s.scope,raiseId:function(v){return h(n.Monad0().Applicative0())(void 0)}})(_)(i))(m.element(s)(_))}}}}}}}}}}}};var N=CM,me=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(a){return function(u){return function(f){var i=function(m){return function(s){return Ut(function(_){return q(r.Monad0().Bind1())(s.ids)(function(v){return q(r.Monad0().Bind1())(s.ids)(function(D){return Wr(Er)(r.Monad0().Bind1())(m.raiseId(v))(function(){var l=Vv(n)(D)(d.value)(f);return p(r.Monad0().Bind1().Apply0().Functor0())(j(r.Monad0().Bind1().Apply0())(_(s.deleteFromCache({id:v}))))(Et(I(P(r.Monad0().Applicative0()))(Ft(r.Monad0().Applicative0())(s.makePursx({id:v,parent:m.parent,cache:l.cache,pxScope:D,scope:m.scope,html:No(t)(u),verb:No(e)(a)})))(l.element(m)(s)))(_))})})})})}};return new U(i)}}}}}}}}},$t=function(t){return function(){return function(){return function(e){return function(r){return me(t)({reflectType:function(){return"~"}})()()(r)(e)(d.value)}}}}};function og(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function ig(t){return function(e){return function(r){return r[t]=e,r}}}var Jv=ei;var jv=function(){return function(){return function(t){return function(e){return function(r){return function(n){return ig(Sr(t)(e))(r)(n)}}}}}};var Xv=et,fg=function(t){return function(e){return t(og(e))}},cg=Tt(fg)({});var bt=function(){return function(){return{defaults:Tt(Xy()())}}},EM=function(t){return t.defaults},At={convertRecordOptions:function(t){return function(e){return function(r){return Z(Xv)}}}},_g=function(t){return t.convertRecordOptions},pa=function(t){return t.convertOptionsWithDefaults},yt=function(){return function(t){return{convertOptions:function(e){return function(r){return cg(_g(t)(e)(d.value)(r))}}}}},hM=function(t){return t.convertOptions},kt=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var a=EM(e)(n),u=hM(t)(r);return function(f){return a(u(f))}}}}}},TM=function(t){return t.convertOption},z=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(a){return function(u){return xo(Jv)(jv()()(r)(d.value)(TM(e)(n)(d.value)(pi(r)()(d.value)(u))))(_g(t)(n)(d.value)(u))}}}}}}}}}};var FM=function(){return function(){return function(){return function(t){return function(e){return function(r){return V_(r.type)(t)?ja(r.type)(t)(r.value):e(r)}}}}}};var Qe=function(){return function(t){return function(e){return function(r){return{type:Sr(t)(e),value:r}}}}};var $M=function(t){return mu("Data.Variant: pattern match failure ["+(t.type+"]"))},Tr=function(){return function(){return function(){return function(t){return FM()()()(t)($M)}}}};var UM=function(t){return t};var o_=function(){return Qe()({reflectSymbol:function(){return"nothing"}})(d.value)(void 0)}();var pn=function(){var t=Qe()({reflectSymbol:function(){return"just"}})(d.value);return function(e){return UM(t(e))}}();var i_={foldl:function(t){return function(e){return function(r){return Tr()()()({just:function(n){return t(e)(n)},nothing:function(n){return e}})(r)}}},foldr:function(t){return function(e){return function(r){return Tr()()()({just:function(n){return t(n)(e)},nothing:function(n){return e}})(r)}}},foldMap:function(t){return up(i_)(t)}};var Kv=function(){var t=mp(Bm);return function(e){return vp(t(e))}}();var Wz=typeof Array.from=="function",Gz=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",Bz=typeof String.prototype.fromCodePoint=="function",Uz=typeof String.prototype.codePointAt=="function";var yu=void 0;var Qp=function(t){return t.toInt},kg=function(t){return function(e){return Qp(t)(yu)}};var Qa={toInt:function(t){return 8}},gg={Nat0:function(){return Qa}},Uo={toInt:function(t){return 7}},Cg={Nat0:function(){return Uo}},qo={toInt:function(t){return 6}},Sg={Nat0:function(){return qo}},$a={toInt:function(t){return 5}},Kp={Nat0:function(){return $a}},zn={toInt:function(t){return 4}},ua={Nat0:function(){return zn}},Vn={toInt:function(t){return 3}},ku={Nat0:function(){return Vn}},Jn={toInt:function(t){return 2}},gu={Nat0:function(){return Jn}},jn={toInt:function(t){return 1}},Cu={Nat0:function(){return jn}},xr={toInt:function(t){return 0}};var Me=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var so={Nat0:function(){return Uo},Nat1:function(){return Qa}};var mo={Nat0:function(){return qo},Nat1:function(){return Qa}};var vo={Nat0:function(){return $a},Nat1:function(){return Qa}};var Do={Nat0:function(){return zn},Nat1:function(){return Qa}};var sa={Nat0:function(){return zn},Nat1:function(){return $a}};var bo={Nat0:function(){return Vn},Nat1:function(){return Qa}};var ma={Nat0:function(){return Vn},Nat1:function(){return $a}};var Ao={Nat0:function(){return Jn},Nat1:function(){return Qa}};var va={Nat0:function(){return Jn},Nat1:function(){return $a}};var yo={Nat0:function(){return jn},Nat1:function(){return Qa}};var Da={Nat0:function(){return jn},Nat1:function(){return $a}};var ko={Nat0:function(){return xr},Nat1:function(){return Qa}};var da={Nat0:function(){return xr},Nat1:function(){return $a}};var Eg={Nat0:function(){return xr},Nat1:function(){return Qa}};var Zv={Nat0:function(){return xr},Nat1:function(){return Uo}};var tD={Nat0:function(){return xr},Nat1:function(){return qo}};var c_={Nat0:function(){return xr},Nat1:function(){return $a}};var Ha={Nat0:function(){return xr},Nat1:function(){return zn}};var An={Nat0:function(){return xr},Nat1:function(){return Vn}};var yn={Nat0:function(){return xr},Nat1:function(){return Jn}};var kn={Nat0:function(){return xr},Nat1:function(){return jn}},Su={Nat0:function(){return xr},Nat1:function(){return xr}};var hg=ai;var Yp=function(t){return t};var l_=function(t){return function(){return function(e){return function(r){return e[Qp(t)(r)]}}}};var Zp=function(t){return function(e){var r=kg(t)(d.value),n=function(){return r===0?[]:dn(0)(r-1|0)}();return p(xe)(e)(n)}};var Hu=[];var we=function(t){return function(e){return function(r){return Gi(e)(r)}}};var gn={first:function(t){return function(e){return new nt(t(e.value0),e.value1)}},second:p(ni),Profunctor0:function(){return xa}},Xn=function(t){return t.second},ts=function(t){return t.first};var Aw=function(t){return function(e){return function(r){return function(n){return Lo(r)(t)(e)(n)}}}};var $g=function(){return function(){return function(t){return Aw(Pu())(Pu())(t)}}};var Mg=function(){return function(){return function(t){return $g()()(t)}}};var gw=function(t){return function(e){return function(r){return Lo(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(ts(e)(r))}}},wg=function(t){return function(e){return function(r){return gw(function(n){return new nt(t(n),function(a){return e(n)(a)})})(r)}}};var Og=function(t){return function(){return function(){return function(e){return function(r){return wg(pi(t)()(e))(Tt(jy(t)()()(e)))(r)}}}}};var Pg=function(t){return t};var Tw=JSON.parse;var xw=JSON.stringify;var es=function(t){return t};var rs=function(t){return t};var ns=function(t){return function(e){return t(e)}},__=function(t){return{map:function(e){return ns(p(t)(p(vf)(e)))}}};var nD=function(t){return{Applicative0:function(){return p_(t)},Bind1:function(){return aD(t)}}},aD=function(t){return{bind:function(e){return function(r){return q(t.Bind1())(e)(La(function(){var n=h(t.Applicative0());return function(a){return n(Yt.create(a))}}())(function(n){var a=r(n);return a}))}},Apply0:function(){return Rg(t)}}},Rg=function(t){return{apply:Qu(nD(t)),Functor0:function(){return __(t.Bind1().Apply0().Functor0())}}},p_=function(t){return{pure:function(){var e=h(t.Applicative0());return function(r){return es(e(Zt.create(r)))}}(),Apply0:function(){return Rg(t)}}};var Ng=function(t){return{throwError:function(){var e=h(t.Applicative0());return function(r){return es(e(Yt.create(r)))}}(),Monad0:function(){return nD(t)}}};var uD=function(t){return function(e){return{alt:function(r){return function(n){return q(e.Bind1())(r)(function(a){if(a instanceof Zt)return h(e.Applicative0())(new Zt(a.value0));if(a instanceof Yt)return q(e.Bind1())(n)(function(u){if(u instanceof Zt)return h(e.Applicative0())(new Zt(u.value0));if(u instanceof Yt)return h(e.Applicative0())(new Yt(dt(t)(a.value0)(u.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[u.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[a.constructor.name])})}},Functor0:function(){return __(e.Bind1().Apply0().Functor0())}}}};var oD=function(){var t=wn();return function(e){return t(rs(e))}}();function Wg(t,e,r){return t==null?e:r(t)}var tn=function(t){return Wg(t,V.value,B.create)};function s_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Vg=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var _D=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Jg=ft;var pD=function(t){var e=_o(Ng(t));return function(r){return e(Kv(r))}};var sD=function(t){return function(e){return function(r){if(s_(r)===e)return h(p_(t))(Jg(r));if(Ye)return pD(t)(new _D(e,s_(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var mD=function(t){return sD(t)("String")};var os=function(){function t(){}return t.value=new t,t}(),is=function(){function t(){}return t.value=new t,t}(),Qg=function(){function t(){}return t.value=new t,t}(),Kg=function(){function t(){}return t.value=new t,t}(),DD=function(){function t(){}return t.value=new t,t}(),Yg=function(){function t(){}return t.value=new t,t}(),Zg=function(){function t(){}return t.value=new t,t}();var tC=function(t){return t},eC=function(t){return t};var rC=function(t){return t};var nC=function(t){return t};var aC=function(t){return t};var uC=function(t){return t},oC=function(t){return t},iC=function(t){return t},fC=function(t){return t},cC=function(t){return t};var dD=function(){function t(){}return t.value=new t,t}(),lC=function(){function t(){}return t.value=new t,t}(),_C=function(){function t(){}return t.value=new t,t}(),bD=function(){function t(){}return t.value=new t,t}(),pC=function(){function t(){}return t.value=new t,t}();var fs=function(t){return t};var gc=function(t){return t};var AD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Sn=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),cs=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Ke=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),fO=function(t){return t},ls=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),_s=function(){function t(){}return t.value=new t,t}(),sC=function(t){return t};var cO=function(t){return t},m_=function(t){return t};var Pf={toAudioOnOff:Z(et)};var If=function(t){return t.toAudioParameter},mC=function(t){return t.toAudioOnOff};var vC=function(t){return AD.create(fO(t))};var ps=function(){return Pg(function(){var t=Mg()()(xa),e=Og({reflectSymbol:function(){return"o"}})()()(d.value)(gn);return function(r){return t(e(r))}}())},DC=ft;var lO=function(){var t=Qe()({reflectSymbol:function(){return"unit"}})(d.value);return function(e){return m_(t(e))}}();var _O=function(t){return function(e){return{toAudioParameter:function(r){return lO(r)}}}},dC=function(t){return function(e){return{toAudioParameter:function(){var r=If(_O(t)(e));return function(n){return r(cO(function(a){return{u:a}}(n)))}}()}}},bC=function(){return Qe()({reflectSymbol:function(){return"2x"}})(d.value)(void 0)}(),AC=function(){var t=Qe()({reflectSymbol:function(){return"sudden"}})(d.value);return function(e){return m_(t(e))}}();var yC={toAudioParameter:AC},ss={toAudioParameter:function(t){return AC({n:t})}},yD=function(){return Qe()({reflectSymbol:function(){return"step"}})(d.value)(void 0)}();var kD=function(){return Qe()({reflectSymbol:function(){return"on"}})(d.value)(void 0)}(),v_={x:kD,o:0},_t=function(){return Y(E(o))(fn()(Qe()({reflectSymbol:function(){return"onOff"}})(d.value)(v_)))};var kC=function(){return Qe()({reflectSymbol:function(){return"off"}})(d.value)(void 0)}();var pO=function(){var t=Qe()({reflectSymbol:function(){return"numeric"}})(d.value);return function(e){return m_(t(e))}}();var Nr={toAudioParameter:pO};var Ho=function(){return Qe()({reflectSymbol:function(){return"linear"}})(d.value)(void 0)}();var gC=function(){return Qe()({reflectSymbol:function(){return"exponential"}})(d.value)(void 0)}(),sO=function(){var t=Qe()({reflectSymbol:function(){return"envelope"}})(d.value);return function(e){return m_(t(e))}}();var Rn={toAudioParameter:sO},mO=function(){var t=Qe()({reflectSymbol:function(){return"cancel"}})(d.value);return function(e){return m_(t(e))}}();var CC={toAudioParameter:mO};var vO=function(){function t(){}return t.value=new t,t}(),DO=function(){function t(){}return t.value=new t,t}(),dO=function(){function t(){}return t.value=new t,t}(),bO=function(){function t(){}return t.value=new t,t}(),AO=function(){function t(){}return t.value=new t,t}(),yO=function(){function t(){}return t.value=new t,t}(),kO=function(){function t(){}return t.value=new t,t}(),gO=function(){function t(){}return t.value=new t,t}(),CO=function(){function t(){}return t.value=new t,t}(),SO=function(){function t(){}return t.value=new t,t}(),EO=function(){function t(){}return t.value=new t,t}(),hO=function(){function t(){}return t.value=new t,t}(),TO=function(){function t(){}return t.value=new t,t}(),xO=function(){function t(){}return t.value=new t,t}(),vi=function(t){return{toPeriodicOscSpec:function(e){return Qe()({reflectSymbol:function(){return"realImg"}})(d.value)({real:Yp(e.value0),img:Yp(e.value1)})}}};var ms={toInitializeTriangleOsc:function(t){return cC(function(e){return{frequency:e}}(t))}};var SC={toInitializeStereoPanner:function(t){return fC(function(e){return{pan:e}}(t))}};var Cc={toInitializeSquareOsc:function(t){return iC(function(e){return{frequency:e}}(t))}};var Zi={toInitializeSinOsc:function(t){return oC(function(e){return{frequency:e}}(t))}};var EC={toInitializeSawtoothOsc:function(t){return uC(function(e){return{frequency:e}}(t))}};var gD={toInitializeRecorder:function(t){return tC(function(e){return{cb:e}}(t))}};var D_={toInitializeMicrophone:function(t){return eC(function(e){return{microphone:e}}(t))}};var hC=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(a){return{feedforward:bv(Bu)(Pu()(r.value0)),feedback:bv(Bu)(Pu()(r.value1))}}}}}}};var it={toInitializeGain:function(t){return aC(function(e){return{gain:e}}(t))}};var TC={toInitializeConvolver:function(t){return rC(function(e){return{buffer:e}}(t))}},vs={toInitializeConstant:function(t){return nC(function(e){return{offset:e}}(t))}};var FO={convertOption:function(t){return function(e){return Z(et)}}},d_={convertOption:function(t){return function(e){return Z(et)}}},xC={convertOption:function(t){return function(e){return Z(et)}}},FC={convertOption:function(t){return function(e){return pn}}},$C={convertOption:function(t){return function(e){return Z(et)}}},Di={convertOption:function(t){return function(e){return Z(et)}}},Sc={convertOption:function(t){return function(e){return Z(et)}}},Ec={convertOption:function(t){return function(e){return Z(et)}}},hc={convertOption:function(t){return function(e){return Z(et)}}},Tc={convertOption:function(t){return function(e){return Z(et)}}},xc={convertOption:function(t){return function(e){return Z(et)}}},MC={convertOption:function(t){return function(e){return Z(et)}}},wC={convertOption:function(t){return function(e){return Z(et)}}},OC={convertOption:function(t){return function(e){return Z(et)}}},CD={convertOption:function(t){return function(e){return Z(et)}}},Rf={convertOption:function(t){return function(e){return Z(et)}}},b_={convertOption:function(t){return function(e){return Z(et)}}},A_={convertOption:function(t){return function(e){return Z(et)}}};var Fc={convertOption:function(t){return function(e){return Z(et)}}},PC={convertOption:function(t){return function(e){return Z(et)}}},IC={convertOption:function(t){return function(e){return Z(et)}}},RC={convertOption:function(t){return function(e){return Z(et)}}},SD={convertOption:function(t){return function(e){return Z(et)}}};var NC={convertOption:function(t){return function(e){return Z(et)}}},ED={convertOption:function(t){return function(e){return Z(et)}}},En={convertOption:function(t){return function(e){return Z(et)}}},sn={convertOption:function(t){return function(e){return Z(et)}}},hD={convertOption:function(t){return function(e){return Z(et)}}},Ds={convertOption:function(t){return function(e){return Z(et)}}},$O=function(t){return t.toPeriodicOscSpec},di=function(t){return{convertOption:function(e){return function(r){return $O(t)}}}},TD=function(t){return t.toInitializeWaveShaper},LC=function(t){return t.toInitializeTriangleOsc},WC=function(t){return t.toInitializeStereoPanner},GC=function(t){return t.toInitializeSquareOsc},BC=function(t){return t.toInitializeSinOsc},UC=function(t){return t.toInitializeSawtoothOsc},qC=function(t){return t.toInitializeRecorder},xD=function(t){return t.toInitializePlayBuf},HC=function(t){return t.toInitializePeriodicOsc},zC=function(t){return t.toInitializePeaking},VC=function(t){return t.toInitializeNotch},JC=function(t){return t.toInitializeMicrophone},jC=function(t){return t.toInitializeLowshelf},FD=function(t){return t.toInitializeLowpass},$D=function(t){return t.toInitializeLoopBuf},XC=function(t){return t.toInitializeIIRFilter},QC=function(t){return t.toInitializeHighshelf},MD=function(t){return t.toInitializeHighpass},KC=function(t){return t.toInitializeGain},YC=function(t){return t.toInitializeDynamicsCompressor},wD=function(t){return t.toInitializeDelay},ZC=function(t){return t.toInitializeConvolver},tS=function(t){return t.toInitializeConstant},OD=function(t){return t.toInitializeBandpass},PD=function(t){return t.toInitializeAllpass};var MO={oversample:bC},wO=function(t){return{toInitializeWaveShaper:function(e){return pa(t)(vO.value)(MO)(e)}}},eS={toInitializeWaveShaper:function(){var t=TD(wO(kt(yt()(z(At)(FO)()()()({reflectSymbol:function(){return"curve"}})))(bt()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},OO={bufferOffset:0,playbackRate:1,duration:o_},y_=function(t){return{toInitializePlayBuf:function(e){return pa(t)(DO.value)(OO)(e)}}},za={toInitializePlayBuf:function(){var t=xD(y_(kt(yt()(z(At)(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},PO={},bi=function(t){return{toInitializePeriodicOsc:function(e){return pa(t)(dO.value)(PO)(e)}}},IO={q:1,gain:0},$c=function(t){return{toInitializePeaking:function(e){return pa(t)(bO.value)(IO)(e)}}};var RO={q:1},Mc=function(t){return{toInitializeNotch:function(e){return pa(t)(AO.value)(RO)(e)}}};var NO={gain:0},rS=function(t){return{toInitializeLowshelf:function(e){return pa(t)(yO.value)(NO)(e)}}};var LO={q:1},ID=function(t){return{toInitializeLowpass:function(e){return pa(t)(kO.value)(LO)(e)}}},ds={toInitializeLowpass:function(){var t=FD(ID(kt(yt()(z(At)(CD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},WO={loopStart:0,loopEnd:0,playbackRate:1,duration:o_},Nf=function(t){return{toInitializeLoopBuf:function(e){return pa(t)(gO.value)(WO)(e)}}},ge={toInitializeLoopBuf:function(){var t=$D(Nf(kt(yt()(z(At)(Rf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},GO={gain:0},nS=function(t){return{toInitializeHighshelf:function(e){return pa(t)(CO.value)(GO)(e)}}};var BO={q:1},RD=function(t){return{toInitializeHighpass:function(e){return pa(t)(SO.value)(BO)(e)}}},Ka={toInitializeHighpass:function(){var t=MD(RD(kt(yt()(z(At)(SD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},UO=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),aS=function(t){return{toInitializeDynamicsCompressor:function(e){return pa(t)(EO.value)(UO)(e)}}},qO={maxDelayTime:1},ND=function(t){return{toInitializeDelay:function(e){return pa(t)(hO.value)(qO)(e)}}},en={toInitializeDelay:function(){var t=wD(ND(kt(yt()(z(At)(ED)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},HO={q:1},mn=function(t){return{toInitializeBandpass:function(e){return pa(t)(TO.value)(HO)(e)}}},LD={toInitializeBandpass:function(){var t=OD(mn(kt(yt()(z(At)(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},zO={q:1},bs=function(t){return{toInitializeAllpass:function(e){return pa(t)(xO.value)(zO)(e)}}},WD={toInitializeAllpass:function(){var t=PD(bs(kt(yt()(z(At)(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};function GD(t){return()=>t.slice()}function BD(t){return e=>r=>()=>{r[t]=e}}function UD(t){return()=>t.slice()}var zo=function(){function t(){this.head=null,this.last=null,this.size=0}function e(_,v){this.queue=_,this.value=v,this.next=null,this.prev=null}function r(_){this.draining=!1,this.error=null,this.value=_,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function a(_){try{_()}catch(v){setTimeout(function(){throw v},0)}}function u(_,v){var D=new e(_,v);switch(_.size){case 0:_.head=D;break;case 1:D.prev=_.head,_.head.next=D,_.last=D;break;default:D.prev=_.last,_.last.next=D,_.last=D}return _.size++,D}function f(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.last,_.head.next=null,_.last=null;break;default:v=_.last,_.last=v.prev,_.last.next=null}return v.prev=null,v.queue=null,_.size--,v.value}function i(_){var v;switch(_.size){case 0:return null;case 1:v=_.head,_.head=null;break;case 2:v=_.head,_.last.prev=null,_.head=_.last,_.last=null;break;default:v=_.head,_.head=v.next,_.head.prev=null}return v.next=null,v.queue=null,_.size--,v.value}function m(_){if(_.queue!==null){if(_.queue.last===_){f(_.queue);return}if(_.queue.head===_){i(_.queue);return}_.prev&&(_.prev.next=_.next),_.next&&(_.next.prev=_.prev),_.queue.size--,_.queue=null,_.value=null,_.next=null,_.prev=null}}function s(_,v){if(!v.draining){var D=v.puts,l=v.takes,g=v.reads,ot,vt,Wt,re,he;for(v.draining=!0;;){if(ot=null,vt=null,Wt=null,re=v.value,he=g.size,v.error!==null){for(re=_.left(v.error);ot=i(D);)a(ot.cb(re));for(;vt=i(g);)a(vt(re));for(;Wt=i(l);)a(Wt(re));break}if(re===n&&(ot=i(D))&&(v.value=re=ot.value),re!==n){for(Wt=i(l);he--&&(vt=i(g));)a(vt(_.right(re)));Wt!==null&&(v.value=n,a(Wt(_.right(re))))}if(ot!==null&&a(ot.cb(_.right(void 0))),v.value===n&&D.size===0||v.value!==n&&l.size===0)break}v.draining=!1}}return r.EMPTY=n,r.putLast=u,r.takeLast=f,r.takeHead=i,r.deleteCell=m,r.drainVar=s,r}();function k_(){return new zo(zo.EMPTY)}function uS(t,e,r){return function(){var n=zo.putLast(e.takes,r);return zo.drainVar(t,e),function(){zo.deleteCell(n)}}}function oS(t,e,r){return function(){return r.value===zo.EMPTY&&r.error===null?(r.value=e,zo.drainVar(t,r),!0):!1}}function iS(t,e){return function(){var r=e.value;return r===zo.EMPTY?t.nothing:(e.value=zo.EMPTY,zo.drainVar(t,e),t.just(r))}}var QO=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),KO=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),YO=function(){function t(){}return t.value=new t,t}();var qD=function(){return{left:Yt.create,right:Zt.create,nothing:V.value,just:B.create,killed:QO.create,filled:KO.create,empty:YO.value}}();var fS=function(t){return function(e){return uS(qD,t,e)}},As=function(t){return function(e){return oS(qD,t,e)}};var cS=function(t){return iS(qD,t)};var ZO=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},_S=function(){function t(){}return t.value=new t,t}(),pS=function(){function t(){}return t.value=new t,t}(),tP=function(){function t(){}return t.value=new t,t}();var eP=function(){function t(){}return t.value=new t,t}();var ys={convertOption:function(t){return function(e){return Z(et)}}},ks={convertOption:function(t){return function(e){return Z(et)}}};var rP=function(t){return t.toInitializeAnalyser};var nP=function(){return{cb:function(t){return h(c)(h(c)(void 0))},fftSize:DD.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:bD.value,channelInterpretation:dD.value}}(),gs=function(t){return{toInitializeAnalyser:function(e){return pa(t)(eP.value)(nP)(e)}}};var aP=function(t){return function(e){var r=JC(t)(e),n=function(a){return function(u){return Ut(function(f){return function(){var m=u.ids();return a.raiseId(m)(),p(x)(function(s){return j(tt)(f(u.deleteFromCache({id:m})))(s)})(Tt(Et)(f)(Ft(c)(u.makeMicrophone({id:m,parent:a.parent,scope:a.scope,microphone:r.microphone}))))()}})}};return new Ke(n)}},g_=function(t){return aP(t)};var Pt=function(t){return function(e){return function(r){return qr(t)(e)($(C(c)))(r)}}},qr=function(t){return function(e){return function(r){return function(n){var a=KC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeGain({id:_,parent:f.parent,scope:f.scope,gain:a.gain})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({gain:sS(594)(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},Ur=function(t){return function(e){var r=function(n){return n(t)(e)};return function(n){if(n instanceof Sn)return Qr(Xt)(C(c))(Ur(t)(e))(n.value0);if(n instanceof cs)return br(E(o))(p(k)(Ur(t)(e))(n.value0));if(n instanceof Ke)return r(n.value0);if(n instanceof AD)return Ut(function(a){return function(){var f=It(o)(Gr(Lu))(),i=Et(n.value0)(function(m){return function(){var _=e.ids(),v=It(o)(Gr(h(c)(void 0)))(),D=e.ids(),l=It(o)(Gr(h(c)(void 0)))(),g=It(o)(Gr(V.value))(),ot=It(o)(Gr(h(c)(void 0)))(),vt=e.ids(),Wt=It(o)(Gr(_S.value))(),re=Et(m)(function(he){return function(){var lt=It(o)($r(Wt))();if(he instanceof _s&&lt instanceof pS){Kt(x)(It(o)(an(tP.value)(Wt)))();var oe=j(tt)(j(tt)(j(tt)(j(tt)(q(hr)(It(o)($r(g)))(rr(c)(Je)(function(Gt){return ea(c)(i_)(t.parent)(function(mr){return a(e.disconnectXFromY({from:Gt,to:mr}))})})))(Zn(hr)(It(o)($r(v)))))(Zn(hr)(It(o)($r(l)))))(Kt(x)(It(o)(ha(fc(_))(f)))))(Kt(x)(It(o)(ha(fc(D))(f))));return j(tt)(Kt(x)(It(o)(an(oe)(ot))))(oe)()}if(he instanceof ls&&lt instanceof _S){Kt(x)(It(o)(an(pS.value)(Wt)))();var ke=Et(Ur({parent:t.parent,scope:vt,raiseId:function(Gt){return Kt(x)(It(o)(an(new B(Gt))(g)))}})(e)(function(){return he.value0 instanceof Ke?he.value0:Pt(it)(1)([he.value0])}()))(a)();return Kt(x)(It(o)(ha(Wu(D)(ke))(f)))(),Kt(x)(It(o)(an(ke)(l)))()}return void 0}})();return Kt(x)(It(o)(an(re)(v)))(),Kt(x)(It(o)(ha(Wu(_)(re))(f)))(),Zn(hr)(It(o)($r(ot)))()}})();return function(){return q(hr)(It(o)($r(f)))(nr(Sf)(j(tt))(h(c)(void 0)))(),i()}}});throw new Error("Failed pattern match at WAGS.Control (line 1771, column 53 - line 1846, column 20): "+[n.constructor.name])}}},sS=ZO("tmpResolveAU","WAGS.Control",function(){var t=function(){var f=Qe()({reflectSymbol:function(){return"unit"}})(d.value);return function(i){return gc(f(i))}}(),e=function(){var f=Qe()({reflectSymbol:function(){return"sudden"}})(d.value);return function(i){return gc(f(i))}}(),r=function(){var f=Qe()({reflectSymbol:function(){return"numeric"}})(d.value);return function(i){return gc(f(i))}}(),n=function(){var f=Qe()({reflectSymbol:function(){return"envelope"}})(d.value);return function(i){return gc(f(i))}}(),a=function(){var f=Qe()({reflectSymbol:function(){return"cancel"}})(d.value);return function(i){return gc(f(i))}}(),u=function(f){return function(i){return function(m){return function(s){return Tr()()()({numeric:function(){var _=Ft(c);return function(v){return _(m(r(v)))}}(),envelope:function(){var _=Ft(c);return function(v){return _(m(n(v)))}}(),cancel:function(){var _=Ft(c);return function(v){return _(m(a(v)))}}(),sudden:function(){var _=Ft(c);return function(v){return _(m(e(v)))}}(),unit:function(_){var v=Pt(it)(1)([_.u]);return Ut(function(D){return function(){var g=k_();return Et(I(P(c))(Ur({parent:o_,scope:f,raiseId:function(ot){return Kt(x)(As(ot)(g))}})(i)(v))(Ut(function(ot){return function(){return Kt(x)(fS(g)(function(Wt){if(Wt instanceof Yt)return Tf(Wt.value0);if(Wt instanceof Zt)return ot(m(t({i:Wt.value0})));throw new Error("Failed pattern match at WAGS.Control (line 1744, column 39 - line 1747, column 66): "+[Wt.constructor.name])}))(),h(c)(void 0)}})))(D)()}})}})(s)}}}};return u}),ir=sS(1723),uP=function(t){return function(e){return function(r){return function(n){var a=rP(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeAnalyser({id:_,parent:f.parent,scope:f.scope,cb:a.cb,fftSize:Ev(2)(function(){if(a.fftSize instanceof os)return 7;if(a.fftSize instanceof is)return 8;if(a.fftSize instanceof Qg)return 9;if(a.fftSize instanceof Kg)return 10;if(a.fftSize instanceof DD)return 11;if(a.fftSize instanceof Yg)return 12;if(a.fftSize instanceof Zg)return 13;throw new Error("Failed pattern match at WAGS.Control (line 192, column 21 - line 199, column 34): "+[a.fftSize.constructor.name])}()),maxDecibels:a.maxDecibels,minDecibels:a.minDecibels,smoothingTimeConstant:a.smoothingTimeConstant,channelCount:a.channelCount,channelCountMode:function(){if(a.channelCountMode instanceof pC)return"explicit";if(a.channelCountMode instanceof bD)return"max";if(a.channelCountMode instanceof _C)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 205, column 35 - line 208, column 46): "+[a.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(a.channelInterpretation instanceof dD)return"speakers";if(a.channelInterpretation instanceof lC)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 209, column 40 - line 211, column 41): "+[a.channelInterpretation.constructor.name])}()})))(I(P(c))(p(k)(function(v){return Tr()()()({cb:function(D){return i.setAnalyserNodeCb({id:_,cb:D})}})(v)})(r))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},Cs=function(t){return function(e){return uP(t)(e)($(C(c)))}},mS=function(t){return function(e){return function(r){var n=ZC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeConvolver({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer})))(Ur({parent:pn(s),scope:u.scope,raiseId:Ot(jr(Xr(un)))})(f)(new Sn(r)))))()}})}};return new Ke(a)}}},Ya=function(t){var e=function(r){return function(n){return Ut(function(a){return function(){var f=It(o)(Gr(V.value))(),i=t(new Ke(function(m){return function(s){return Ut(function(_){return function(){return function(){var l=It(o)($r(f))();if(l instanceof V)return void 0;if(l instanceof B)return ea(c)(i_)(m.parent)(function(g){return $n(c)(l.value0!==g)(j(tt)(m.raiseId(l.value0))(_(n.connectXToY({from:l.value0,to:g}))))})();throw new Error("Failed pattern match at WAGS.Control (line 1651, column 36 - line 1655, column 82): "+[l.constructor.name])}(),h(c)(void 0)}})}}));return Et(Ur({parent:r.parent,scope:r.scope,raiseId:function(m){return function(){return r.raiseId(m)(),Kt(x)(It(o)(an(new B(m))(f)))()}}})(n)(i))(a)()}})}};return new Ke(e)};var oP=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(a){var u=XC(t)(n)(e)(r),f=function(i){return function(m){return Ut(function(s){return function(){var v=m.ids();return i.raiseId(v)(),p(x)(function(D){return j(tt)(s(m.deleteFromCache({id:v})))(D)})(Tt(Et)(s)(I(P(c))(Ft(c)(m.makeIIRFilter({id:v,parent:i.parent,scope:i.scope,feedforward:kf()(u.feedforward),feedback:kf()(u.feedback)})))(Ur({parent:pn(v),scope:i.scope,raiseId:Ot(jr(Xr(un)))})(m)(new Sn(a)))))()}})}};return new Ke(f)}}}}}}},vS=function(){return function(){return function(t){return oP()()(t)(d.value)(d.value)}}},HD=function(t){return function(e){return function(r){var n=qC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeRecorder({id:s,parent:u.parent,scope:u.scope,cb:n.cb})))(Ur({parent:pn(s),scope:u.scope,raiseId:Ot(jr(Xr(un)))})(f)(r))))()}})}};return new Ke(a)}}},iP=function(t){return function(e){return Ut(function(r){return function(){var a=e.ids();return r(e.makeSpeaker({id:a}))(),Et(Ur({parent:pn(a),scope:"toplevel",raiseId:Ot(jr(Xr(un)))})(e)(new Sn(t)))(r)()}})}},Lf=iP,fP=function(t){return function(e){return function(r){var n=$D(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeLoopBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(br(E(o))(p(k)(function(_){return Tr()()()({buffer:function(v){return Ft(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:ir(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),loopStart:function(v){return Ft(c)(f.setLoopStart({id:s,loopStart:v}))},loopEnd:function(v){return Ft(c)(f.setLoopEnd({id:s,loopEnd:v}))},onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},_e=function(t){return fP(t)};var cP=function(t){return function(e){return function(r){var n=HC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makePeriodicOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency,spec:n.spec})))(br(E(o))(p(k)(function(_){return Tr()()()({frequency:ir(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))},spec:function(v){return Ft(c)(f.setPeriodicOsc({id:s,spec:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},Ai=function(t){return cP(t)};var lP=function(t){return function(e){return function(r){var n=xD(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makePlayBuf({id:s,parent:u.parent,scope:u.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(br(E(o))(p(k)(function(_){return Tr()()()({buffer:function(v){return Ft(c)(f.setBuffer({id:s,buffer:v}))},playbackRate:ir(u.scope)(f)(function(v){return f.setPlaybackRate(function(D){return{id:s,playbackRate:D}}(v))}),bufferOffset:function(v){return Ft(c)(f.setBufferOffset({id:s,bufferOffset:v}))},onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))},duration:function(v){return Ft(c)(f.setDuration({id:s,duration:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},Qn=function(t){return lP(t)};var _P=function(t){return function(e){return function(r){var n=UC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeSawtoothOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(E(o))(p(k)(function(_){return Tr()()()({frequency:ir(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},DS=function(t){return _P(t)};var pP=function(t){return function(e){return function(r){var n=BC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeSinOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(E(o))(p(k)(function(_){return Tr()()()({frequency:ir(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},Wf=function(t){return pP(t)},dS=function(t){return function(e){return Wf(t)(e)($(C(c)))}},sP=function(t){return function(e){return function(r){var n=GC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeSquareOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(E(o))(p(k)(function(_){return Tr()()()({frequency:ir(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},C_=function(t){return sP(t)},bS=function(t){return function(e){return C_(t)(e)($(C(c)))}},mP=function(t){return function(e){return function(r){var n=LC(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeTriangleOsc({id:s,parent:u.parent,scope:u.scope,frequency:n.frequency})))(br(E(o))(p(k)(function(_){return Tr()()()({frequency:ir(u.scope)(f)(function(v){return f.setFrequency(function(D){return{id:s,frequency:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},Ss=function(t){return mP(t)};var vP=function(t){return function(e){return function(r){return function(n){var a=PD(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeAllpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},S_=function(t){return function(e){return function(r){return vP(t)(e)($(C(c)))(r)}}},zD=function(t){return function(e){return function(r){return function(n){var a=OD(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeBandpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},hn=function(t){return function(e){return function(r){return zD(t)(e)($(C(c)))(r)}}},E_=function(t){return function(e){return function(r){return function(n){var a=wD(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeDelay({id:_,parent:f.parent,scope:f.scope,delayTime:a.delayTime,maxDelayTime:a.maxDelayTime})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({delayTime:ir(f.scope)(i)(function(D){return i.setDelay(function(l){return{id:_,delayTime:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},go=function(t){return function(e){return function(r){return E_(t)(e)($(C(c)))(r)}}},DP=function(t){return function(e){return function(r){return function(n){var a=YC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeDynamicsCompressor({id:_,parent:f.parent,scope:f.scope,threshold:a.threshold,ratio:a.ratio,knee:a.knee,attack:a.attack,release:a.release})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({threshold:ir(f.scope)(i)(function(D){return i.setThreshold(function(l){return{id:_,threshold:l}}(D))}),ratio:ir(f.scope)(i)(function(D){return i.setRatio(function(l){return{id:_,ratio:l}}(D))}),knee:ir(f.scope)(i)(function(D){return i.setKnee(function(l){return{id:_,knee:l}}(D))}),attack:ir(f.scope)(i)(function(D){return i.setAttack(function(l){return{id:_,attack:l}}(D))}),release:ir(f.scope)(i)(function(D){return i.setRelease(function(l){return{id:_,release:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},AS=function(t){return function(e){return DP(t)(e)($(C(c)))}},dP=function(){return function(t){return function(e){return function(r){return function(n){var a=function(u){return function(f){return Ut(function(i){return function(){var s=GD(p(xe)(T(""))(kf()(r)))(),_=dr(Xt)(C(c))(ao(oi)(function(){var Wt=function(re){return function(he){var gr=re,lt=!1,oe;function ke(Gt,mr){if(mr instanceof Ke)return lt=!0,mr.value0({parent:pn("@portal@"),scope:e(u.scope),raiseId:function(Tu){return BD(Gt)(Tu)(s)}})(f);gr=Gt,he=Pt(it)(1)([mr])}for(;!lt;)oe=ke(gr,he);return oe}};return Wt}())(kf()(r))),v=Et(_)(i)(),D=It(o)(Gr(h(c)(void 0)))(),l=p(x)(ft)(UD(s))(),g=p(Xm)(function(Wt){return new Ke(function(re){return function(he){return Ut(function(gr){return function(){return re.raiseId(Wt)(),ea(c)(i_)(re.parent)(function(oe){return gr(he.connectXToY({from:Wt,to:oe}))})(),h(c)(void 0)}})}})})(l),ot=Ur(u)(f)(n(g)(ft)),vt=Et(ot)(i)();return Kt(x)(It(o)(an(vt)(D)))(),function(){return v(),$n(c)(!t)(ea(c)(Xt)(kf()(l))(function(re){return i(f.deleteFromCache({id:re}))}))(),Zn(hr)(It(o)($r(D)))()}}})}};return new Ke(a)}}}}},bP=function(){return function(t){return dP()(!1)(Z(et))(t)}},Ma=function(t){return function(e){return bP()(Py(t))(fo(xa)(Iy()()()()()({reflectType:function(){return 0}})(d.value))(e))}};var VD=function(t){return function(e){return function(r){return function(n){var a=MD(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeHighpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},wc=function(t){return function(e){return function(r){return VD(t)(e)($(C(c)))(r)}}},AP=function(t){return function(e){return function(r){return function(n){var a=QC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeHighshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:ir(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},yS=function(t){return function(e){return function(r){return AP(t)(e)($(C(c)))(r)}}},kS=function(t){return function(e){return function(r){return function(n){var a=FD(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeLowpass({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},Oc=function(t){return function(e){return function(r){return kS(t)(e)($(C(c)))(r)}}},yP=function(t){return function(e){return function(r){return function(n){var a=jC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeLowshelf({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,gain:a.gain})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),gain:ir(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},gS=function(t){return function(e){return function(r){return yP(t)(e)($(C(c)))(r)}}},kP=function(t){return function(e){return function(r){return function(n){var a=VC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeNotch({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},Pc=function(t){return function(e){return function(r){return kP(t)(e)($(C(c)))(r)}}},gP=function(t){return function(e){return function(r){return function(n){var a=WC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makeStereoPanner({id:_,parent:f.parent,scope:f.scope,pan:a.pan})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({pan:ir(f.scope)(i)(function(D){return i.setPan(function(l){return{id:_,pan:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},CS=function(t){return function(e){return gP(t)(e)($(C(c)))}},CP=function(t){return function(e){return function(r){return function(n){var a=zC(t)(e),u=function(f){return function(i){return Ut(function(m){return function(){var _=i.ids();return f.raiseId(_)(),p(x)(function(v){return j(tt)(m(i.deleteFromCache({id:_})))(v)})(Tt(Et)(m)(I(P(c))(Ft(c)(i.makePeaking({id:_,parent:f.parent,scope:f.scope,frequency:a.frequency,q:a.q,gain:a.gain})))(I(P(c))(br(E(o))(p(k)(function(v){return Tr()()()({frequency:ir(f.scope)(i)(function(D){return i.setFrequency(function(l){return{id:_,frequency:l}}(D))}),q:ir(f.scope)(i)(function(D){return i.setQ(function(l){return{id:_,q:l}}(D))}),gain:ir(f.scope)(i)(function(D){return i.setGain(function(l){return{id:_,gain:l}}(D))})})(v)})(r)))(Ur({parent:pn(_),scope:f.scope,raiseId:Ot(jr(Xr(un)))})(i)(new Sn(n))))))()}})}};return new Ke(u)}}}},Ic=function(t){return function(e){return function(r){return CP(t)(e)($(C(c)))(r)}}},SS=function(t){return function(e){return function(r){var n=TD(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeWaveShaper({id:s,parent:u.parent,scope:u.scope,curve:n.curve,oversample:n.oversample})))(Ur({parent:pn(s),scope:u.scope,raiseId:Ot(jr(Xr(un)))})(f)(new Sn(r)))))()}})}};return new Ke(a)}}},SP=function(t){return function(e){return function(r){var n=tS(t)(e),a=function(u){return function(f){return Ut(function(i){return function(){var s=f.ids();return u.raiseId(s)(),p(x)(function(_){return j(tt)(i(f.deleteFromCache({id:s})))(_)})(Tt(Et)(i)(I(P(c))(Ft(c)(f.makeConstant({id:s,parent:u.parent,scope:u.scope,offset:n.offset})))(br(E(o))(p(k)(function(_){return Tr()()()({offset:ir(u.scope)(f)(function(v){return f.setOffset(function(D){return{id:s,offset:D}}(v))}),onOff:function(v){return Ft(c)(f.setOnOff({id:s,onOff:v}))}})(_)})(r)))))()}})}};return new Ke(a)}}},Es=function(t){return SP(t)};function JD(){window.scrollTo(0,0)}var Co=function(t){return t.sequential},Nn=function(t){return t.parallel};var Tn=function(t){return function(e){return function(r){return new U(J(t)("button")(e)(new H(r)))}}};var wa=function(){var t={},e="Pure",r="Throw",n="Catch",a="Sync",u="Async",f="Bind",i="Bracket",m="Fork",s="Sequential",_="Map",v="Apply",D="Alt",l="Cons",g="Resume",ot="Release",vt="Finalizer",Wt="Finalized",re="Forked",he="Fiber",gr="Thunk";function lt(qt,je,Hr,fr){this.tag=qt,this._1=je,this._2=Hr,this._3=fr}function oe(qt){var je=function(Hr,fr,ie){return new lt(qt,Hr,fr,ie)};return je.tag=qt,je}function ke(qt){return new lt(e,void 0)}function Gt(qt){try{qt()}catch(je){setTimeout(function(){throw je},0)}}function mr(qt,je,Hr){try{return je(Hr())}catch(fr){return qt(fr)}}function Tu(qt,je,Hr){try{return je(Hr)()}catch(fr){return Hr(qt(fr))(),ke}}var ju=function(){var qt=1024,je=0,Hr=0,fr=new Array(qt),ie=!1;function St(){var tr;for(ie=!0;je!==0;)je--,tr=fr[Hr],fr[Hr]=void 0,Hr=(Hr+1)%qt,tr();ie=!1}return{isDraining:function(){return ie},enqueue:function(tr){var Pe,Lr;je===qt&&(Lr=ie,St(),ie=Lr),fr[(Hr+je)%qt]=tr,je++,ie||St()}}}();function Mi(qt){var je={},Hr=0,fr=0;return{register:function(ie){var St=Hr++;ie.onComplete({rethrow:!0,handler:function(tr){return function(){fr--,delete je[St]}}})(),je[St]=ie,fr++},isEmpty:function(){return fr===0},killAll:function(ie,St){return function(){if(fr===0)return St();var tr=0,Pe={};function Lr(vr){Pe[vr]=je[vr].kill(ie,function(rn){return function(){delete Pe[vr],tr--,qt.isLeft(rn)&&qt.fromLeft(rn)&&setTimeout(function(){throw qt.fromLeft(rn)},0),tr===0&&St()}})()}for(var Kn in je)je.hasOwnProperty(Kn)&&(tr++,Lr(Kn));return je={},Hr=0,fr=0,function(vr){return new lt(a,function(){for(var rn in Pe)Pe.hasOwnProperty(rn)&&Pe[rn]()})}}}}}var ou=0,Dn=1,Yo=2,zf=3,Vf=4,Fr=5,Zo=6;function Jf(qt,je,Hr){var fr=0,ie=ou,St=Hr,tr=null,Pe=null,Lr=null,Kn=null,vr=null,rn=0,lf=0,xu=null,wi=!0;function Oi(fe){for(var pe,Ue,ze;;)switch(pe=null,Ue=null,ze=null,ie){case Yo:ie=Dn;try{St=Lr(St),Kn===null?Lr=null:(Lr=Kn._1,Kn=Kn._2)}catch(fa){ie=Fr,tr=qt.left(fa),St=null}break;case zf:qt.isLeft(St)?(ie=Fr,tr=St,St=null):Lr===null?ie=Fr:(ie=Yo,St=qt.fromRight(St));break;case Dn:switch(St.tag){case f:Lr&&(Kn=new lt(l,Lr,Kn)),Lr=St._2,ie=Dn,St=St._1;break;case e:Lr===null?(ie=Fr,St=qt.right(St._1)):(ie=Yo,St=St._1);break;case a:ie=zf,St=mr(qt.left,qt.right,St._1);break;case u:ie=Vf,St=Tu(qt.left,St._1,function(fa){return function(){fr===fe&&(fr++,ju.enqueue(function(){fr===fe+1&&(ie=zf,St=fa,Oi(fr))}))}});return;case r:ie=Fr,tr=qt.left(St._1),St=null;break;case n:Lr===null?vr=new lt(l,St,vr,Pe):vr=new lt(l,St,new lt(l,new lt(g,Lr,Kn),vr,Pe),Pe),Lr=null,Kn=null,ie=Dn,St=St._1;break;case i:rn++,Lr===null?vr=new lt(l,St,vr,Pe):vr=new lt(l,St,new lt(l,new lt(g,Lr,Kn),vr,Pe),Pe),Lr=null,Kn=null,ie=Dn,St=St._1;break;case m:ie=zf,pe=Jf(qt,je,St._2),je&&je.register(pe),St._1&&pe.run(),St=qt.right(pe);break;case s:ie=Dn,St=D0(qt,je,St._1);break}break;case Fr:if(Lr=null,Kn=null,vr===null)ie=Zo,St=Pe||tr||St;else switch(pe=vr._3,ze=vr._1,vr=vr._2,ze.tag){case n:Pe&&Pe!==pe&&rn===0?ie=Fr:tr&&(ie=Dn,St=ze._2(qt.fromLeft(tr)),tr=null);break;case g:Pe&&Pe!==pe&&rn===0||tr?ie=Fr:(Lr=ze._1,Kn=ze._2,ie=Yo,St=qt.fromRight(St));break;case i:rn--,tr===null&&(Ue=qt.fromRight(St),vr=new lt(l,new lt(ot,ze._2,Ue),vr,pe),(Pe===pe||rn>0)&&(ie=Dn,St=ze._3(Ue)));break;case ot:vr=new lt(l,new lt(Wt,St,tr),vr,Pe),ie=Dn,Pe&&Pe!==pe&&rn===0?St=ze._1.killed(qt.fromLeft(Pe))(ze._2):tr?St=ze._1.failed(qt.fromLeft(tr))(ze._2):St=ze._1.completed(qt.fromRight(St))(ze._2),tr=null,rn++;break;case vt:rn++,vr=new lt(l,new lt(Wt,St,tr),vr,Pe),ie=Dn,St=ze._1;break;case Wt:rn--,ie=Fr,St=ze._1,tr=ze._2;break}break;case Zo:for(var Vr in xu)xu.hasOwnProperty(Vr)&&(wi=wi&&xu[Vr].rethrow,Gt(xu[Vr].handler(St)));xu=null,Pe&&tr?setTimeout(function(){throw qt.fromLeft(tr)},0):qt.isLeft(St)&&wi&&setTimeout(function(){if(wi)throw qt.fromLeft(St)},0);return;case ou:ie=Dn;break;case Vf:return}}function zr(fe){return function(){if(ie===Zo)return wi=wi&&fe.rethrow,fe.handler(St)(),function(){};var pe=lf++;return xu=xu||{},xu[pe]=fe,function(){xu!==null&&delete xu[pe]}}}function de(fe,pe){return function(){if(ie===Zo)return pe(qt.right(void 0))(),function(){};var Ue=zr({rethrow:!1,handler:function(){return pe(qt.right(void 0))}})();switch(ie){case ou:Pe=qt.left(fe),ie=Zo,St=Pe,Oi(fr);break;case Vf:Pe===null&&(Pe=qt.left(fe)),rn===0&&(ie===Vf&&(vr=new lt(l,new lt(vt,St(fe)),vr,Pe)),ie=Fr,St=null,tr=null,Oi(++fr));break;default:Pe===null&&(Pe=qt.left(fe)),rn===0&&(ie=Fr,St=null,tr=null)}return Ue}}function Re(fe){return function(){var pe=zr({rethrow:!1,handler:fe})();return ie===ou&&Oi(fr),pe}}return{kill:de,join:Re,onComplete:zr,isSuspended:function(){return ie===ou},run:function(){ie===ou&&(ju.isDraining()?Oi(fr):ju.enqueue(function(){Oi(fr)}))}}}function ti(qt,je,Hr,fr){var ie=0,St={},tr=0,Pe={},Lr=new Error("[ParAff] Early exit"),Kn=null,vr=t;function rn(zr,de,Re){var fe=de,pe=null,Ue=null,ze=0,Vr={},fa,rl;t:for(;;)switch(fa=null,fe.tag){case re:if(fe._3===t&&(fa=St[fe._1],Vr[ze++]=fa.kill(zr,function(d0){return function(){ze--,ze===0&&Re(d0)()}})),pe===null)break t;fe=pe._2,Ue===null?pe=null:(pe=Ue._1,Ue=Ue._2);break;case _:fe=fe._2;break;case v:case D:pe&&(Ue=new lt(l,pe,Ue)),pe=fe,fe=fe._1;break}if(ze===0)Re(qt.right(void 0))();else for(rl=0,fa=ze;rl<fa;rl++)Vr[rl]=Vr[rl]();return Vr}function lf(zr,de,Re){var fe,pe,Ue,ze,Vr,fa;qt.isLeft(zr)?(fe=zr,pe=null):(pe=zr,fe=null);t:for(;;){if(Ue=null,ze=null,Vr=null,fa=null,Kn!==null)return;if(de===null){fr(fe||pe)();return}if(de._3!==t)return;switch(de.tag){case _:fe===null?(de._3=qt.right(de._1(qt.fromRight(pe))),pe=de._3):de._3=fe;break;case v:if(Ue=de._1._3,ze=de._2._3,fe){if(de._3=fe,Vr=!0,fa=tr++,Pe[fa]=rn(Lr,fe===Ue?de._2:de._1,function(){return function(){delete Pe[fa],Vr?Vr=!1:Re===null?lf(fe,null,null):lf(fe,Re._1,Re._2)}}),Vr){Vr=!1;return}}else{if(Ue===t||ze===t)return;pe=qt.right(qt.fromRight(Ue)(qt.fromRight(ze))),de._3=pe}break;case D:if(Ue=de._1._3,ze=de._2._3,Ue===t&&qt.isLeft(ze)||ze===t&&qt.isLeft(Ue))return;if(Ue!==t&&qt.isLeft(Ue)&&ze!==t&&qt.isLeft(ze))fe=pe===Ue?ze:Ue,pe=null,de._3=fe;else if(de._3=pe,Vr=!0,fa=tr++,Pe[fa]=rn(Lr,pe===Ue?de._2:de._1,function(){return function(){delete Pe[fa],Vr?Vr=!1:Re===null?lf(pe,null,null):lf(pe,Re._1,Re._2)}}),Vr){Vr=!1;return}break}Re===null?de=null:(de=Re._1,Re=Re._2)}}function xu(zr){return function(de){return function(){delete St[zr._1],zr._3=de,lf(de,zr._2._1,zr._2._2)}}}function wi(){var zr=Dn,de=Hr,Re=null,fe=null,pe,Ue;t:for(;;)switch(pe=null,Ue=null,zr){case Dn:switch(de.tag){case _:Re&&(fe=new lt(l,Re,fe)),Re=new lt(_,de._1,t,t),de=de._2;break;case v:Re&&(fe=new lt(l,Re,fe)),Re=new lt(v,t,de._2,t),de=de._1;break;case D:Re&&(fe=new lt(l,Re,fe)),Re=new lt(D,t,de._2,t),de=de._1;break;default:Ue=ie++,zr=Fr,pe=de,de=new lt(re,Ue,new lt(l,Re,fe),t),pe=Jf(qt,je,pe),pe.onComplete({rethrow:!1,handler:xu(de)})(),St[Ue]=pe,je&&je.register(pe)}break;case Fr:if(Re===null)break t;Re._1===t?(Re._1=de,zr=Dn,de=Re._2,Re._2=t):(Re._2=de,de=Re,fe===null?Re=null:(Re=fe._1,fe=fe._2))}for(vr=de,Ue=0;Ue<ie;Ue++)St[Ue].run()}function Oi(zr,de){Kn=qt.left(zr);var Re;for(var fe in Pe)if(Pe.hasOwnProperty(fe)){Re=Pe[fe];for(fe in Re)Re.hasOwnProperty(fe)&&Re[fe]()}Pe=null;var pe=rn(zr,vr,de);return function(Ue){return new lt(u,function(ze){return function(){for(var Vr in pe)pe.hasOwnProperty(Vr)&&pe[Vr]();return ke}})}}return wi(),function(zr){return new lt(u,function(de){return function(){return Oi(zr,de)}})}}function D0(qt,je,Hr){return new lt(u,function(fr){return function(){return ti(qt,je,Hr,fr)}})}return lt.EMPTY=t,lt.Pure=oe(e),lt.Throw=oe(r),lt.Catch=oe(n),lt.Sync=oe(a),lt.Async=oe(u),lt.Bind=oe(f),lt.Bracket=oe(i),lt.Fork=oe(m),lt.Seq=oe(s),lt.ParMap=oe(_),lt.ParApply=oe(v),lt.ParAlt=oe(D),lt.Fiber=Jf,lt.Supervisor=Mi,lt.Scheduler=ju,lt.nonCanceler=ke,lt}(),ES=wa.Pure,wP=wa.Throw;function hS(t){return function(e){return e.tag===wa.Pure.tag?wa.Pure(t(e._1)):wa.Bind(e,function(r){return wa.Pure(t(r))})}}function TS(t){return function(e){return wa.Bind(t,e)}}var xS=wa.Sync;function FS(t){return function(e){return wa.ParMap(t,e)}}function $S(t){return function(e){return wa.ParApply(t,e)}}function MS(t){return function(e){return wa.ParAlt(t,e)}}var Rc=wa.Async;function wS(t,e){return function(){return wa.Fiber(t,null,e)}}var OP=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return wa.Async(function(a){return function(){var u=t(n,a(r()));return function(){return wa.Sync(function(){return r(e(n,u))})}}})}}(),OS=wa.Seq;var IP=function(t){return function(e){return function(r){var n=Co(t),a=rr(t.Applicative1())(e)(function(){var u=Nn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},PS=function(t){return function(e){return function(r){var n=Co(t),a=ra(e)(t.Applicative1())(function(){var u=Nn(t);return function(f){return u(r(f))}}());return function(u){return n(a(u))}}}},IS=function(t){return function(e){return IP(t)(e)(Z(et))}};var RP=function(t){return t};var NS=function(t){return t};var T_=function(t){return t.toDuration};var LS={fromDuration:Cm()()(RP)(function(t){return t*1e3}),toDuration:Cm()()(NS)(function(t){return t/1e3})};var WS=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}};var LP=function(t){return t};var Lc={map:FS},yi={map:hS};var WP=function(){var t=function(n){if(n instanceof Zt)return n.value0;if(n instanceof Yt)return mu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof Yt)return n.value0;if(n instanceof Zt)return mu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof Yt)return!0;if(n instanceof Zt)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:Yt.create,right:Zt.create}}(),GP=function(t){return wS(WP,t)},So=function(t){return function(){var r=GP(t)();return r.run(),r}},Jo=function(){var t=Kt(x);return function(e){return t(So(e))}}();var ki={apply:$S,Functor0:function(){return Lc}};var jD={Applicative0:function(){return ba},Bind1:function(){return Rr}},Rr={bind:TS,Apply0:function(){return XD(0)}},ba={pure:ES,Apply0:function(){return XD(0)}},XD=WS("applyAff","Effect.Aff",function(){return{apply:Qu(jD),Functor0:function(){return yi}}}),GS=XD(71);var Ir={liftEffect:xS,Monad0:function(){return jD}},BS=function(){var t=sr(Ir);return function(e){return LP(T(t(e)))}}(),US=function(t){return Rc(function(e){return p(x)(BS)(t.join(e))})};var qS=function(t){return function(e){return q(Rr)(sr(Ir)(e.isSuspended))(function(r){return r?sr(Ir)(Kt(x)(e.kill(t,T(h(c)(void 0))))):Rc(function(n){return p(x)(BS)(e.kill(t,n))})})}};var Ln={parallel:ft,sequential:OS,Monad0:function(){return jD},Applicative1:function(){return BP(0)}},BP=WS("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Nn(Ln),e=h(ba);return function(r){return t(e(r))}}(),Apply0:function(){return ki}}});var UP={append:function(t){return function(e){return function(r){return IS(Ln)(Xt)([t(r),e(r)])}}}};var qP=T(h(ba)(void 0)),HS={mempty:qP,Semigroup0:function(){return UP}};var zS={alt:MS,Functor0:function(){return Lc}};var VS=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),x_=function(){function t(){}return t.value=new t,t}(),Gf=function(){function t(){}return t.value=new t,t}(),F_=function(){function t(){}return t.value=new t,t}(),Bf=function(){function t(){}return t.value=new t,t}(),$_=function(){function t(){}return t.value=new t,t}(),M_=function(){function t(){}return t.value=new t,t}(),JS=function(){function t(){}return t.value=new t,t}(),hs=function(){function t(){}return t.value=new t,t}(),Ts=function(){function t(){}return t.value=new t,t}(),w_=function(){function t(){}return t.value=new t,t}(),O_=function(){function t(){}return t.value=new t,t}(),jS=function(){function t(){}return t.value=new t,t}(),Wc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),QD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var HP="numeric",zP="sudden",VP="unit",JP="cancel",jP="step",XP="linear",QP="exponential",KP="envelope",XS=function(t,e,r,n){if(r.type===zP)t.value=r.value.n;else if(r.type===VP)e.id&&ZP(e.id,n),n.units[r.value.i].main.connect(t),e.id=r.value.i;else if(r.type===HP)t[r.value.t.type===jP?"setValueAtTime":r.value.t.type===XP?"linearRampToValueAtTime":r.value.t.type===QP?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===JP)r.value.hold?t.cancelAndHoldAtTime(r.value.o):t.cancelScheduledValues(r.value.o);else if(r.type===KP){let a=r.value.o;t.cancelScheduledValues(Math.max(0,a)),t.setValueCurveAtTime(r.value.p,a,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},YP=function(t,e,r,n,a){return n[r]||(n[r]={}),XS(e.parameters.get(r),n[r],a,t)},zu=function(t,e,r,n,a){return n[r]||(n[r]={}),XS(e[r],n[r],a,t)},Ar=function(t,e,r){r.scopes[e]||(r.scopes[e]=[]),r.scopes[e].push(t),r.units[t].scope=e},yr=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},kr=function(t,e,r){e.type==="just"&&QS(t,e.value,r)},QS=function(t,e,r){var n=function(){r.units[t].audioOutgoing.push(e),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var a={f:n};e!==t&&!r.units[e]&&(a.w=e),r.toConnect[t].push(a);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var a={f:n};e!==t&&!r.units[t]&&(a.w=t),r.toConnect[e].push(a);return}n()};function KD(t){return function(e){return function(){delete e.units[t.id]}}}function YD(t){return function(e){return function(){QS(t.from,t.to,e)}}}var ZP=function(t,e){if(e.units[t].scope==="@fan@")return;let r=e.units[t].scope;e.scopes[r].forEach(n=>{delete e.units[n]}),delete e.scopes[r]};function ZD(t){return function(e){return function(){var r=t.from,n=t.to;if(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(u){return u!==n}),e.units[r].main.disconnect(e.units[n].main),e.units[n].se&&e.units[r].main.disconnect(e.units[n].se),e.units[r].scope==="@fan@")return;let a=e.units[r].scope;e.scopes[a].forEach(u=>{delete e.units[u]}),delete e.scopes[a]}}}function td(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ed(t){return function(e){return function(){var r=t.id,n=t.cb,a=new AnalyserNode(e.context,t),u=n(a)();e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:n,analyser:u,main:e.context.createGain(),se:a},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function rd(t){return function(e){return function(){var r=t.id,n=t.options;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,n.name,{numberOfInputs:n.numberOfInputs,numberOfOutputs:n.numberOfOutputs,outputChannelCount:n.outputChannelCount,parameterData:n.parameterData,processorOptions:n.processorOptions})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function nd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ad(t){return function(e){return function(){var r=t.id,n=function(u,f){return new ConstantSourceNode(u,f)},a={offset:t.offset};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ud(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:t.buffer})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function od(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:t.delayTime,maxDelayTime:t.maxDelayTime})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function id(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:t.knee,ratio:t.ratio,threshold:t.threshold,attack:t.attack,release:t.release})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}var fd=function(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}};function cd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function ld(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:t.frequency,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function _d(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:t.feedforward,feedback:t.feedback})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function pd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new AudioBufferSourceNode(u,f)},a={loop:!0,buffer:t.buffer,loopStart:t.loopStart,loopEnd:t.loopEnd,playbackRate:t.playbackRate};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function sd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:t.q,frequency:t.frequency})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function md(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:t.frequency,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function vd(t){return function(e){return function(){var r=t.id,n=t.element,a=function(){var u=e.context.createMediaElementSource(n);return u};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:a,resumeClosure:{},main:a()},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Dd(t){return function(e){return function(){var r=t.id;e.units[t.id]={main:e.context.createMediaStreamSource(t.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function dd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:t.frequency,Q:t.q})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function bd(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:t.frequency,Q:t.q,gain:t.gain})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Ad(t){return function(e){return function(){var r=t.id,n=function(u,f){var i={frequency:f.frequency,periodicWave:f.spec.type==="wave"?f.spec.value:nb(e.context)(f.spec.value.real)(f.spec.value.img)()},m=new OscillatorNode(u,i);return m},a={frequency:t.frequency,type:"custom",spec:t.spec};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function yd(t){return function(e){return function(){var r=t.id,n=function(u,f){var i={loop:f.loop,buffer:f.buffer,playbackRate:f.playbackRate};return new AudioBufferSourceNode(u,i)},a={loop:!1,buffer:t.buffer,playbackRate:t.playbackRate,bufferOffset:t.bufferOffset,duration:t.duration};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function kd(t){return function(e){return function(){var r=t.id,n=t.cb,a=e.context.createMediaStreamDestination(),u=new MediaRecorder(a.stream);n(u)(),u.start(),e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:n,recorder:u,main:e.context.createGain(),se:a},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function gd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"sawtooth"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Cd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"sine"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Sd(t){return function(e){return function(){e.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}}}}function Ed(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:t.pan})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function hd(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"square"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Td(t){return function(e){return function(){var r=t.id,n=function(u,f){return new OscillatorNode(u,f)},a={frequency:t.frequency,type:"triangle"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:a,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,a)},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function xd(t){return function(e){return function(){var r=t.id,n=t.curve,a=t.oversample;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:n,oversample:a.type})},Ar(r,t.scope,e),yr(r,e),kr(r,t.parent,e)}}}function Fd(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function $d(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var a=r;e.units[n].recorderOrig=r;var u=new MediaRecorder(e.units[n].se);a(u)(),u.start()}}}}function Md(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function wd(t){return function(e){return function(){var r=t.id,n=t.paramName,a=t.paramValue;YP(e,e.units[r].main,n,e.units[r].controllers,a)}}}var Vu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function Od(t){return function(e){return function(){var r=t.id,n=t.gain;zu(e,e.units[r].main,"gain",e.units[r].controllers,n),Vu(n,e.units[r],"gain")}}}function Pd(t){return function(e){return function(){var r=t.id,n=t.q;zu(e,e.units[r].main,"Q",e.units[r].controllers,n),Vu(n,e.units[r],"Q")}}}function Id(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function Rd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function Nd(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function Ld(t){return function(e){return function(){var r=t.id,n=t.pan;zu(e,e.units[r].main,"pan",e.units[r].controllers,n),Vu(n,e.units[r],"pan")}}}function Wd(t){return function(e){return function(){var r=t.id,n=t.threshold;zu(e,e.units[r].main,"threshold",e.units[r].controllers,n),Vu(n,e.units[r],"threshold")}}}function Gd(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function Bd(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function Ud(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function qd(t){return function(e){return function(){var r=t.id,n=t.duration;e.units[r].duration=n}}}function Hd(t){return function(e){return function(){var r=t.id,n=t.release;zu(e,e.units[r].main,"release",e.units[r].controllers,n),Vu(n,e.units[r],"release")}}}function zd(t){return function(e){return function(){var r=t.id,n=t.offset;zu(e,e.units[r].main,"offset",e.units[r].controllers,n),Vu(n,e.units[r],"offset")}}}function Vd(t){return function(e){return function(){var r=t.id,n=t.ratio;zu(e,e.units[r].main,"ratio",e.units[r].controllers,n),Vu(n,e.units[r],"ratio")}}}function Jd(t){return function(e){return function(){var r=t.id,n=t.attack;zu(e,e.units[r].main,"attack",e.units[r].controllers,n),Vu(n,e.units[r],"attack")}}}function jd(t){return function(e){return function(){var r=t.id,n=t.knee;zu(e,e.units[r].main,"knee",e.units[r].controllers,n),Vu(n,e.units[r],"knee")}}}function Xd(t){return function(e){return function(){var r=t.id,n=t.delayTime;zu(e,e.units[r].main,"delayTime",e.units[r].controllers,n),Vu(n,e.units[r],"delayTime")}}}function Qd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;zu(e,e.units[r].main,"playbackRate",e.units[r].controllers,n),Vu(n,e.units[r],"playbackRate")}}}function Kd(t){return function(e){return function(){var r=t.id,n=t.frequency;zu(e,e.units[r].main,"frequency",e.units[r].controllers,n),Vu(n,e.units[r],"frequency")}}}function Yd(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?tI(r)(n)(e)():n.x.type==="off"&&eI(r)(n)(e)()}}}var tI=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].audioOutgoing.length;n++){var a=r.units[t].audioOutgoing[n];r.units[t].main.connect(r.units[a].main),r.units[a].se&&r.units[t].main.connect(r.units[a].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?r.units[t].resume.duration.type==="just"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration.value):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},eI=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function Zd(t){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}function xs(t){return function(){t.stop()}}function tb(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(a){n.push(a.data)},r.onstop=function(){var a=new Blob(n,{type:t});e(a)(),n=null}}}}}function eb(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function P_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function rb(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var nb=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),a=new Float32Array(r.length),u=0;u<e.length;u++)n[u]=e[u];for(var u=0;u<r.length;u++)a[u]=r[u];return t.createPeriodicWave(n,a,{disableNormalization:!0})}}}};function tf(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},scopes:{},unsu:{},toConnect:{}}}}function ab(t){return function(){t.close()}}function ub(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function ob(t){return function(e){return function(){return t.decodeAudioData(e)}}}function ib(){return new(window.AudioContext||window.webkitAudioContext)}function fb(t){return function(){return t.state}}function I_(t){return function(){return t.currentTime}}function KS(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var aI=function(t){return function(e){return Rc(function(r){return z_(x)(Ot(HS))(KS(e)(function(n){return r(Yt.create(t(n)))()})(function(n){return r(Zt.create(n))()}))})}};var uI=function(t){return La(function(e){return Go("Promise failed, couldn't extract JS Error or String")})(Z(et))(oD(I(uD(Gm)(Yu))(sD(Yu)("Error")(t))(p(__(Mo))(Go)(mD(Yu)(t)))))},YS=aI(uI),Fs=function(t){return q(Rr)(sr(Ir)(t))(YS)};function cb(t){return function(){return URL.createObjectURL(t)}}var ZS=function(t){return function(e){return function(r){return Tt(tb(t))(r)(function(){var n=Yn(hr)(e);return function(a){return n(cb(a))}}())}}};var Uf={ids:p(x)(jt(j_))(Uu),deleteFromCache:KD,disconnectXFromY:ZD,connectXToY:YD,makeAllpass:td,makeAnalyser:ed,makeAudioWorkletNode:rd,makeBandpass:nd,makeConstant:ad,makeConvolver:ud,makeDelay:od,makeDynamicsCompressor:id,makeGain:fd,makeHighpass:cd,makeHighshelf:ld,makeIIRFilter:_d,makeLoopBuf:pd,makeLowpass:sd,makeLowshelf:md,makeMediaElement:vd,makeMicrophone:Dd,makeNotch:dd,makePeaking:bd,makePeriodicOsc:Ad,makePlayBuf:yd,makeRecorder:kd,makeSawtoothOsc:gd,makeSinOsc:Cd,makeSpeaker:Sd,setDuration:qd,makeSquareOsc:hd,makeStereoPanner:Ed,makeTriangleOsc:Td,makeWaveShaper:xd,setAnalyserNodeCb:Fd,setMediaRecorderCb:$d,setWaveShaperCurve:Md,setAudioWorkletParameter:wd,setBuffer:Id,setConvolverBuffer:Rd,setPeriodicOsc:Nd,setOnOff:Yd,setBufferOffset:Ud,setLoopStart:Gd,setLoopEnd:Bd,setRatio:Vd,setOffset:zd,setAttack:Jd,setGain:Od,setQ:Pd,setPan:Ld,setThreshold:Wd,setRelease:Hd,setKnee:jd,setDelay:Xd,setPlaybackRate:Qd,setFrequency:Kd},Ct=function(t){return function(e){return q(Rr)(Fs(ub(e)))(function(){var r=ob(t);return function(n){return Fs(r(n))}}())}},R_=function(t){var e=sr(t);return function(r){return e(fb(r))}};var oa=function(t){return sr(t)(ib)},Ju=function(t){var e=sr(t);return function(r){return e(rb(r))}},xn=function(t){return function(e){return sr(t)(function(){var n=R_(or)(e)();return $n(c)(n!=="closed")(ab(e))()})}},lI=ft,_I=ft,$s=function(t){return function(e){return p(yi)(function(r){return{microphone:function(){return t?h($o)(lI(r)):V.value}(),camera:function(){return e?h($o)(_I(r)):V.value}()}})(Fs(eb(t)(e)))}};var jo=function(){function t(){}return t.value=new t,t}(),Xo=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),hu=function(){function t(){}return t.value=new t,t}(),vn=JD,gi=function(t){return Co(Ln)(I(zS)(Nn(Ln)(q(Rr)(US(t))(sr(Ir))))(Nn(Ln)(qS(Go("We navigated away from the page"))(t))))},Gc=function(t){return function(e){return function(r){return function(n){return I(t)(Y(e)(hu.value))(n)}}}},Oa=function(t){return function(e){return function(r){return function(n){return I(t)(Y(e)(K(pr)(_r.value)(Xe(T(n)))))(p(t.Functor0())(function(a){return K(pr)(_r.value)(Xe(T(j(tt)(a)(n))))})(p(t.Functor0())(function(a){return a.value0})(r)))}}}},Ms=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return p(t)(function(s){return K(pr)(_r.value)(Xe(T(function(){if(s.value0 instanceof jo)return h(c)(void 0);if(s.value0 instanceof Xo)return j(tt)(j(tt)(s.value0.value0)(n(h(c)(void 0))))(a(hu.value));if(s.value0 instanceof hu)return function(){s.value1(),a(jo.value)();var v=So(q(Rr)(oa(Ir))(function(D){return q(Rr)(Ju(Ir)(D))(function(l){return q(Rr)(u(D))(function(g){return sr(Ir)(function(){var vt=f(D)(g)(),Wt=j(tt)(j(tt)(vt)(l))(xn(or)(D));return a(new Xo(Wt))(),Wt})})})}))();return Wr(e)(hr)(n(function(){return a(hu.value)(),Jo(gi(v))()}))(function(){return h(c)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[s.value0.constructor.name])}())))})(Pn(r)(I(r.Plus0().Alt0())(Y(r)(h(c)(void 0)))(p(t)(function(s){return s.value0})(i)))(p(t)(nt.create)(m)))}}}}}}}}},Pa=function(t){return function(e){return function(r){return function(){return t(r)(),e(new VS(r))()}}}},ws=function(t){return function(e){return function(r){return function(n){return function(a){return Zr(o)(function(u){return function(f){var i=Gc(P(c))(E(o))(e)(f);return _c(o)(I(P(c))(Y(E(o))(K(Mp)(Vt.value)("cursor: pointer;")))(Ms(k)(Er)(E(o))(r)(u)(n)(a)(e)(i)))([ln(ue)(p(k)(function(m){if(m instanceof hu)return t;if(m instanceof jo)return"\u23F3";if(m instanceof Xo)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[m.constructor.name])})(i))])}})}}}}},ht=function(t){return function(e){return function(r){return function(n){return Zr(o)(function(a){return function(u){var f=Gc(P(c))(E(o))(t)(u);return Tn(o)(Ms(k)(Er)(E(o))(e)(a)(r)(n)(t)(f))([ln(ue)(p(k)(function(i){if(i instanceof hu)return"Turn on";if(i instanceof jo)return"Loading...";if(i instanceof Xo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[i.constructor.name])})(f))])}})}}}};var Bc=function(t){return function(e){return function(){var n=tf(t)(),a=Et(Lf([new cs(p(k)(function(u){return Sn.create(sC(u))})(e))])(Uf))(function(u){return u(n)})();return a}}};var mt=function(t){return function(e){return function(){var n=tf(t)(),a=Et(Lf(e)(Uf))(function(u){return u(n)})();return a}}},Os=function(t){return function(){var r=oa(or)();return p(x)(function(n){return j(tt)(n)(xn(or)(r))})(mt(r)(t))()}};var pI=function(){return d.value}(),tE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.2
      [ b
      , allpass_ 700.0
          [ allpass_ { frequency: 990.0, q: 20.0 } [ b ]
          , allpass_ 1110.0
              [ b
              , allpass_ { frequency: 2010.0, q: 30.0 } [ b ]
              ]
          ]
      ]
  ]
</code></pre>

  @allpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(d.value)(pI)({allpass:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ma(_e(ge)(a)(_t()))(function(u){return function(f){return Pt(it)(.2)([u,S_(WD)(700)([S_(bs(kt(yt()(z(z(At)(hD)()()()({reflectSymbol:function(){return"q"}}))(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:990,q:20})([u]),S_(WD)(1110)([u,S_(bs(kt(yt()(z(z(At)(hD)()()()({reflectSymbol:function(){return"q"}}))(Ds)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2010,q:30})([u])])])])}})])}})))})}}};function Ci(t){return function(r,n,a){if(n===null)return new t(r);var u=r.byteLength,f=t.BYTES_PER_ELEMENT,i=Math.min(u,n>>>0);if(a===null)return new t(r,i);var m=Math.min((u-i)/f,a);return new t(r,i,m)}}var mI=Ci(Uint8ClampedArray),vI=Ci(Uint32Array),DI=Ci(Uint16Array),eE=Ci(Uint8Array),dI=Ci(Int32Array),bI=Ci(Int16Array),AI=Ci(Int8Array),yI=Ci(Float32Array),kI=Ci(Float64Array);function rE(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var Ps={create:eE,BinaryValue0:function(){}};var Is=function(t){return function(e){return function(){return rE(e)}}};var Uc=yu,qc=yu,Hc=yu,Za=yu,tu=yu,eu=yu,ru=yu,nu=yu;function Rs(t){return t|0}var Si=function(){return window};function oE(t,e,r,n){if(typeof window<"u"){var a=window[r];if(a!=null&&n instanceof a)return e(n)}for(var u=n;u!=null;){var f=Object.getPrototypeOf(u),i=f.constructor.name;if(i===r)return e(n);if(i==="Object")return t;u=f}return t}var Rt=function(t){return function(e){return oE(V.value,B.create,t,e)}};var lb=Rt("HTMLCanvasElement");function iE(t){return function(){return t.body}}var fE=function(){var t=p(x)(tn);return function(e){return t(iE(e))}}();var cE=ft;function qf(t){return function(){return t.valueAsNumber}}var zc=Rt("HTMLInputElement");function pb(t){return function(){return t.document}}function Ns(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var sb=ft;var fR=function(t,e,r){var n=0,a;return function(u){if(n===2)return a;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+u+")",e,u);return n=1,a=r(),n=2,a}},ef=Ut(function(t){return function(){var r=Si(),n=lr(!0)(),a=fR("fx","FRP.Event.Animate",function(){return Kt(x)(Tt(Ns)(r)(function(){var i=wr(n)();return $n(c)(i)(function(){return t(void 0)(),a(19)()})()}))}),u=a(15);return u(),Mn(!1)(n)}});var cR="background-color: rgb(150,30,10);",lR="background-color: rgb(130,60,10);",_R="background-color: rgb(80,90,10);",pR="background-color: rgb(10,130,10);",sR="background-color: rgb(10,100,0);",mR=Zp(Qa)(function(t){return we(Me(Kp)()(Ha)()(c_))(cR)(we(Me(ua)()(An)()(Ha))(lR)(we(Me(ku)()(yn)()(An))(_R)(we(Me(gu)()(kn)()(yn))(pR)(we(Me(Cu)()(Su)()(kn))(sR)(Hu)))))}),vR=function(t){return function(e){return function(r){return function(n){return Cs(gs(kt(yt()(z(z(At)(ks)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))({cb:n,fftSize:is.value})([_e(e)(r)(_t())])}}}},DR=function(){return d.value}(),Ie="background-color: rgb(255,255,255,0.0);",Ne=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return p(t)(function(v){var D=l_(e)()(l_(n)()(v)(m))(s);return D?K(u)(Vt.value)(l_(e)()(l_(n)()(mR)(m))(s)):K(u)(Vt.value)(Ie)})(_)}}}}}}}}}}},dR=function(){return 15/40}(),bR=function(){return 10/40}(),AR=function(){return 7/40}(),yR=function(){return 3/40}(),kR=function(){return 1/40}(),_E=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } [ loopBuf atar bangOn ]</code></pre>

  ~analyser~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))(o)(DR)({analyser:N(rt(Zr(o)(function(n){return function(a){var u=$l(Du(c))(Z(et))(a),f=Gc(P(c))(E(o))(r)(function(m){return m.right}(u)),i=function(m){return m.left}(u);return Be(o)([Tn(o)(I(P(c))(Y(E(o))(K(cc)(Vt.value)("cursor: pointer;")))(Ms(k)(Er)(E(o))(t)(function(m){return n(Zt.create(m))})(function(m){return Ct(m)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(m){return function(s){return function(){var v=lr(V.value)(),D=tf(m)(),l=Lf([vR(ys)(ge)(s)(function(ot){return function(){return Mn(new B(ot))(v)(),Mn(V.value)(v)}})])(Uf),g=Et(I(P(c))(p(k)(Zt.create)(l))(p(k)(Yt.create)(ef)))(function(ot){if(ot instanceof Zt)return ot.value0(D);if(ot instanceof Yt)return function(){var Wt=wr(v)();return ea(c)(Je)(Wt)(function(re){return function(){var gr=P_(re)(),lt=Is(Ps)(gr)(),oe=lr(0)(),ke=lr(0)(),Gt=lr(0)(),mr=lr(0)(),Tu=lr(0)(),ju=lr(0)(),Mi=lr(0)(),ou=lr(0)(),Dn=lr(0)(),Yo=lr(0)(),zf=function(Fr){if(Fr<32)return oe;if(Fr<64)return ke;if(Fr<96)return Gt;if(Fr<128)return mr;if(Fr<168)return Tu;if(Fr<160)return ju;if(Fr<224)return Mi;if(Ye)return ou;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[Fr.constructor.name])};sl(lt)(function(Fr){var Zo=Rs(Fr);return function(){var ti=wr(Yo)();return Df(Le($u)(Zo))(Dn)(),Df(Le($u)(Zo))(zf(ti))(),Df(Le($u)(1))(Yo)()}})();var Vf=ra(hg)(c)(function(Fr){return function(){var Jf=p(x)(qe)(wr(Fr))(),ti=p(x)(Zu(pl)(Jf))(p(x)(qe)(wr(Dn)))();return we(Me(Kp)()(Ha)()(c_))(ti>dR)(we(Me(ua)()(An)()(Ha))(ti>bR)(we(Me(ku)()(yn)()(An))(ti>AR)(we(Me(gu)()(kn)()(yn))(ti>yR)(we(Me(Cu)()(Su)()(kn))(ti>kR)(Hu)))))}})(we(Me(gg)()(Zv)()(Eg))(oe)(we(Me(Cg)()(tD)()(Zv))(ke)(we(Me(Sg)()(c_)()(tD))(Gt)(we(Me(Kp)()(Ha)()(c_))(mr)(we(Me(ua)()(An)()(Ha))(Tu)(we(Me(ku)()(yn)()(An))(ju)(we(Me(gu)()(kn)()(yn))(Mi)(we(Me(Cu)()(Su)()(kn))(ou)(Hu)))))))))();return n(new Yt(Vf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ot.constructor.name])})();return function(){return g(),function(){var Wt=R_(or)(m)();return $n(c)(Wt!=="closed")(xn(or)(m))()}(),n(new Yt(Zp(Qa)(T(Zp($a)(T(!1))))))()}}}})(r)(f)))([ln(ue)(p(k)(function(m){if(m instanceof hu)return"Turn on";if(m instanceof jo)return"Loading...";if(m instanceof Xo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[m.constructor.name])})(f))]),Te(o)(Y(E(o))(K(pt)(Vt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(xr)(ko)(pt)(da)(ko)(nu)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(jn)(yo)(pt)(da)(yo)(ru)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(Jn)(Ao)(pt)(da)(Ao)(eu)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(Vn)(bo)(pt)(da)(bo)(tu)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(zn)(Do)(pt)(da)(Do)(Za)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)($a)(vo)(pt)(da)(vo)(Hc)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(qo)(mo)(pt)(da)(mo)(qc)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(xr)(da)(Uo)(so)(pt)(da)(so)(Uc)(nu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(xr)(ko)(pt)(Da)(ko)(nu)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(jn)(yo)(pt)(Da)(yo)(ru)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(Jn)(Ao)(pt)(Da)(Ao)(eu)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(Vn)(bo)(pt)(Da)(bo)(tu)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(zn)(Do)(pt)(Da)(Do)(Za)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)($a)(vo)(pt)(Da)(vo)(Hc)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(qo)(mo)(pt)(Da)(mo)(qc)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(jn)(Da)(Uo)(so)(pt)(Da)(so)(Uc)(ru)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(xr)(ko)(pt)(va)(ko)(nu)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(jn)(yo)(pt)(va)(yo)(ru)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(Jn)(Ao)(pt)(va)(Ao)(eu)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(Vn)(bo)(pt)(va)(bo)(tu)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(zn)(Do)(pt)(va)(Do)(Za)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)($a)(vo)(pt)(va)(vo)(Hc)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(qo)(mo)(pt)(va)(mo)(qc)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Jn)(va)(Uo)(so)(pt)(va)(so)(Uc)(eu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(xr)(ko)(pt)(ma)(ko)(nu)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(jn)(yo)(pt)(ma)(yo)(ru)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(Jn)(Ao)(pt)(ma)(Ao)(eu)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(Vn)(bo)(pt)(ma)(bo)(tu)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(zn)(Do)(pt)(ma)(Do)(Za)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)($a)(vo)(pt)(ma)(vo)(Hc)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(qo)(mo)(pt)(ma)(mo)(qc)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(Vn)(ma)(Uo)(so)(pt)(ma)(so)(Uc)(tu)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(xr)(ko)(pt)(sa)(ko)(nu)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(jn)(yo)(pt)(sa)(yo)(ru)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(Jn)(Ao)(pt)(sa)(Ao)(eu)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(Vn)(bo)(pt)(sa)(bo)(tu)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(zn)(Do)(pt)(sa)(Do)(Za)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)($a)(vo)(pt)(sa)(vo)(Hc)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(qo)(mo)(pt)(sa)(mo)(qc)(Za)(i)))([]),Te(o)(I(P(c))(Y(E(o))(K(pt)(Vt.value)(Ie)))(Ne(k)(zn)(sa)(Uo)(so)(pt)(sa)(so)(Uc)(Za)(i)))([])])])}})))})}}};var CR=function(){return d.value}(),pE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))(d.value)(CR)({bandpass:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ma(_e(ge)(a)(_t()))(function(u){return function(f){return Pt(it)(.8)([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var ER=function(){return d.value}(),sE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="compression">Compression</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">Compression</a>, when used judiciously, can make certain sounds sit better in a mix, like for example vocals. The <a href="https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode">MDN Web Audio documentation</a> does an excellent job explaining how its parameters work. When used not-judiciously, it makes everything sound loud, and who likes that? So let's use it judiciously, like in the example below. We'll pass an object that only specifies the threshold and otherwise use the default options for the compressor.</p>

  <pre><code>
-- defaultDynamicsCompressor =
--   { ratio: 12.0
--   , attack: 0.003
--   , release: 0.25
--   , knee: 30.0
--   , threshold: -24.0
--   }
run2_
  [ dynamicsCompressor_ { threshold: -50.0 }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~compression~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))(o)(ER)({compression:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([AS(aS(kt(yt()(At))(bt()())))({})([_e(ge)(a)(_t())])])}})))})}}};var ia=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"playbackRate"}})(d.value),n=If(t);return function(a){return e(r(n(a)))}}},Hf=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"onOff"}})(d.value),n=mC(t);return function(a){return e(r(n(a)))}}},mE=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"offset"}})(d.value),n=If(t);return function(a){return e(r(n(a)))}}},vE=function(){var t=fn(),e=Qe()({reflectSymbol:function(){return"loopStart"}})(d.value);return function(r){return t(e(r))}},DE=function(){var t=fn(),e=Qe()({reflectSymbol:function(){return"loopEnd"}})(d.value);return function(r){return t(e(r))}},Fn=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"gain"}})(d.value),n=If(t);return function(a){return e(r(n(a)))}}},Eo=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"frequency"}})(d.value),n=If(t);return function(a){return e(r(n(a)))}}};var Vc=function(){return function(t){var e=fn(),r=Qe()({reflectSymbol:function(){return"delayTime"}})(d.value),n=If(t);return function(a){return e(r(n(a)))}}};var TR=function(){return d.value}(),dE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(R()(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))(o)(TR)({tf:N(ur(ue)("<|>")),txt:N(ur(ue)(`run2_
  [ gain_ 0.5
      [ constant 0.0
          ( bangOn <|>
              ( bang $ offset $ AudioEnvelope
                  { d: 5.0
                  , o: 0.1
                  , p: 0 .. 1920 # mapWithIndex
                      \\i -> const $
                      if i \`mod\` 3 == 0 then 1.0
                      else 0.0
                  }
              )
          )
      ]
  ]`)),constant:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.5)([Es(vs)(0)(I(P(c))(_t())(Y(E(o))(mE()(Rn)({d:5,o:.1,p:ao(oi)(function(u){return T(function(){var f=cu(wo)(u)(3)===0;return f?1:0}())})(dn(0)(1920))}))))])])}})))})}}};var FR=function(){return d.value}(),bE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))(o)(FR)({convolution:N(rt(ht(r)(t)(function(n){return Ht(GS)(p(yi)(function(a){return function(u){return{loop:a,verb:u}}})(Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Ct(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(a){return mt(n)([mS(TC)(a.verb)([_e(ge)(a.loop)(_t())])])}})))})}}};var MR=function(){return d.value}(),AE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))(d.value)(MR)({delay:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([Ma(Qn(za)(a)(_t()))(function(u){return function(f){return Pt(it)(.2)([go(en)(.03)([u]),go(en)(.1)([u]),go(en)(.3)([u]),go(en)(.7)([u])])}})])}})))})}}};var OR=function(){return d.value}(),yE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="gain">Gain</h2>
  <p>The almighty <a href="https://developer.mozilla.org/en-US/docs/Web/API/GainNode">gain</a> node is your friendly neighborhood volume control. Volume in the web-audio API goes from 0 to 1 whereas we hear logarithmically, so when you're using this, make sure to convert between decibels and gain if you want to work with more intuitive units. The conversion formula is as follows:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>And here's a little example of a single gain node:</p>

  <pre><code>run2_
  [ gain_ 0.1
    [ loopBuf buf bangOn ]
  ] </code></pre>

  ~gain~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))(o)(OR)({gain:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([Pt(it)(.1)([_e(ge)(a)(_t())])])}})))})}}};var IR=function(){return d.value}(),kE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))(o)(IR)({highpass:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([wc(Ka)(2e3)([_e(ge)(a)(_t())])])}})))})}}};var NR=function(){return d.value}(),gE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))(o)(NR)({highshelf:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([yS(nS(kt(yt()(z(z(At)(PC)()()()({reflectSymbol:function(){return"gain"}}))(IC)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,gain:.4})([_e(ge)(a)(_t())])])}})))})}}};var WR=function(){return d.value}(),CE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 : 0.0004059599 : 0.00020298 : empty)
          /\\ (1.0126964558 : -1.9991880801 : 0.9873035442 : empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}})(o))(o)(WR)({iirFilterEx:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([vS()()(hC(Bu)(Bu))(new nt(gf()()(20298e-8)(gf()()(.0004059599)(gf()()(20298e-8)(Qm))),gf()()(1.0126964558)(gf()()(-1.9991880801)(gf()()(.9873035442)(Qm)))))([_e(ge)(a)(_t())])])}})))})}}};var BR=function(){return d.value}(),SE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="loopbuf">Looping buffer</h2>

  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">looping buffer</a> is buffered audio that loops. The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start and end and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [ loopBuf
      { buffer: buf
      , playbackRate: 0.5
      , loopStart: 0.1
      , loopEnd: 0.6
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.0
      , loopStart: 0.5
      , loopEnd: 1.2
      }
      bangOn
  , loopBuf
      { buffer: buf
      , playbackRate: 1.7
      }
      bangOn
  ]</code></pre>

  @loopBuf@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))(d.value)(BR)({loopBuf:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(a){return mt(n)([_e(Nf(kt(yt()(z(z(z(z(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Rf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:.5,loopStart:.1,loopEnd:.6})(_t()),_e(Nf(kt(yt()(z(z(z(z(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Rf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:1,loopStart:.5,loopEnd:1.2})(_t()),_e(Nf(kt(yt()(z(z(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Rf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,playbackRate:1.7})(_t())])}})))})}}};var qR=function(){return d.value}(),EE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))(o)(qR)({lowpass:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Oc(ds)(215)([_e(ge)(a)(_t())])])}})))})}}};var zR=function(){return d.value}(),hE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))(o)(zR)({lowshelf:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([gS(rS(kt(yt()(z(z(At)(MC)()()()({reflectSymbol:function(){return"gain"}}))(wC)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:91,gain:.4})([_e(ge)(a)(_t())])])}})))})}}};var JR=function(){return d.value}(),TE=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2 id="microphone">Microphone</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode">microphone</a> will use your microphone if you give the browser permission to do so.</p>

  <blockquote>Make sure to use \u{1F3A7} when you run this example! Otherwise, you'll cause quite a stir in whatever internet cafe, household or public restroom you're perusing this documentation in.</blockquote>

  <pre><code>\\mic -> run2_
  [ case mic of
      Just m -> fix \\i -> gain_ 1.0
        [ microphone m
        , delay_ 0.1 [ gain_ 0.2 [ input i ] ]
        ]
      Nothing -> gain_ 0.02 [ sinOsc_ 440.0 ]
  ]</code></pre>

  @microphone@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))(d.value)(JR)({microphone:N(rt(ht(r)(t)(function(n){return $s(!0)(!1)})(function(n){return function(a){return mt(n)([function(){if(a.microphone instanceof B)return Ya(function(u){return Pt(it)(1)([g_(D_)(a.microphone.value0),go(en)(.1)([Pt(it)(.2)([u])])])});if(a.microphone instanceof V)return Pt(it)(.02)([dS(Zi)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[a.microphone.constructor.name])}()])}})))})}}};var XR=function(){return d.value}(),xE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ pure $ notch_ { frequency: 880.0, q: 5.0 }
    $ pure $ notch_ { frequency: 1200.0, q: 10.0 }
    $ pure $ notch_ { frequency: 2000.0, q: 20.0 }
    $ pure $ notch_ { frequency: 3000.0, q: 30.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))(o)(XR)({notch:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Pc(Mc(kt(yt()(z(z(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})(h(cr)(Pc(Mc(kt(yt()(z(z(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})(h(cr)(Pc(Mc(kt(yt()(z(z(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})(h(cr)(Pc(Mc(kt(yt()(z(z(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})(h(cr)(Pc(Mc(kt(yt()(z(z(At)(Tc)()()()({reflectSymbol:function(){return"q"}}))(xc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})(h(cr)(_e(ge)(a)(_t())))))))))))])}})))})}}};var KR=function(){return d.value}(),FE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ pure $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ pure $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ pure $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))(o)(KR)({peaking:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ic($c(kt(yt()(z(z(z(At)(Sc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1,gain:-20})(h(cr)(Ic($c(kt(yt()(z(z(z(At)(Sc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5,gain:20})(h(cr)(Ic($c(kt(yt()(z(z(z(At)(Sc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10,gain:-20})(h(cr)(Ic($c(kt(yt()(z(z(z(At)(Sc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20,gain:20})(h(cr)(Ic($c(kt(yt()(z(z(z(At)(Sc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"gain"}}))(hc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30,gain:-20})(h(cr)(_e(ge)(a)(_t())))))))))))])}})))})}}};var ZR=function(){return d.value}(),$E=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="periodic">Periodic wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">periodic wave oscillator</a> plays back a custom periodic waveform at a given frequency. The custom waveform must be set as part of the initialization and can be changed after initialization. Note that the change will not go into effect if the oscillator is on: it must be turned off and on again.</p>

  <pre><code>\\buf -> run2_
  [
    gain_ 0.2
      [
        periodicOsc
          { frequency: 140.0
          , spec:
              ( (0.1 +> 0.2 +> 0.3 +> 0.4 +> empty)
                  /\\ (0.4 +> 0.3 +> 0.2 +> 0.1 +> empty)
              )
          }
          bangOn
      ]
  ]</code></pre>

  ~periodic~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(ZR)({periodic:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.2)([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:140,spec:new nt(we(Me(ua)()(An)()(Ha))(.1)(we(Me(ku)()(yn)()(An))(.2)(we(Me(gu)()(kn)()(yn))(.3)(we(Me(Cu)()(Su)()(kn))(.4)(Hu)))),we(Me(ua)()(An)()(Ha))(.4)(we(Me(ku)()(yn)()(An))(.3)(we(Me(gu)()(kn)()(yn))(.2)(we(Me(Cu)()(Su)()(kn))(.1)(Hu)))))})(_t())])])}})))})}}};var eN=function(){return d.value}(),ME=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="playbuf">Playing a buffer</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode">Playback from a buffer</a> is one of the bread-and-butter operations in Web Audio (or any audio). The buffered audio is usually a sound file, but it'll play anything you write to a buffer. Like in the Web Audio API, you can set the buffer's start time and optionally its duration.</p>

  <pre><code>\\buf -> run2_
  [
    playBuf
      { buffer
      , duration: 3.0
      , bufferOffset: 4.2
      }
      bangOn
  ]
</code></pre>

  ~playBuf~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))(o)(eN)({playBuf:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(a){return mt(n)([Qn(y_(kt(yt()(z(z(z(At)(FC)()()()({reflectSymbol:function(){return"duration"}}))(xC)()()()({reflectSymbol:function(){return"bufferOffset"}}))(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:a,duration:3,bufferOffset:4.2})(_t())])}})))})}}};var mb=function(){function t(){}return t.value=new t,t}();var wE={attr:function(t){return function(e){return b({key:"controls",value:L(e)})}}};var vb=function(){function t(){}return t.value=new t,t}();var OE={attr:function(t){return function(e){return b({key:"src",value:L(e)})}}};var Db=function(t){return function(e){return function(r){return new U(J(t)("audio")(e)(new H(r)))}}};var oN=function(t){return function(e){return function(r){return function(n){return HD(t)(n)(g_(e)(r))}}}},iN=function(){return d.value}(),PE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))(o)(iN)({recorder:N(rt(Zr(o)(function(n){return function(a){var u=$l(Du(c))(Z(et))(a),f=$l(Du(c))(Z(et))(function(_){return _.left}(u)),i=function(_){return _.right}(f),m=Gc(P(c))(E(o))(r)(function(_){return _.right}(u)),s=function(_){return _.left}(f);return Be(o)([Tn(o)(I(P(c))(Y(E(o))(K(cc)(Vt.value)("cursor: pointer;")))(p(k)(function(_){return K(pr)(_r.value)(Xe(T(function(){if(_.e instanceof jo)return h(c)(void 0);if(_.e instanceof Xo)return j(tt)(j(tt)(j(tt)(_.e.value0)(t(h(c)(void 0))))(ea(c)(Je)(_.rec)(function(){var v=Yl(Nv);return function(D){return v(xs(D))}}())))(n(Zt.create(hu.value)));if(_.e instanceof hu)return function(){_.cncl();var D=k_();n(new Zt(jo.value))();var l=So(q(Rr)(p(yi)(function(g){return g.microphone})($s(!0)(!1)))(function(g){return sr(Ir)(function(){var vt=Br(h(c)(h(c)(void 0)))(function(Wt){return function(){var he=oa(or)(),gr=tf(he)(),lt=Lf([oN(gD)(D_)(Wt)(function(ke){return function(){return n(new Yt(new Zt(ke)))(),Kt(x)(As(ke)(D))(),ZS("audio/ogg; codecs=opus")(function(mr){return n(Yt.create(Yt.create(mr)))})(ke)()}})])(Uf),oe=Et(lt)(function(ke){return ke(gr)})();return function(){oe(),q(hr)(cS(D))(rr(c)(Je)(function(){var mr=Yl(Nv);return function(Tu){return mr(xs(Tu))}}()))();var Gt=R_(or)(he)();return $n(c)(Gt!=="closed")(xn(or)(he))()}}})(g)();return n(new Zt(new Xo(vt)))(),vt})}))();return t(function(){return n(Zt.create(hu.value))(),Jo(gi(l))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[_.e.constructor.name])}())))})(Pn(E(o))(I(P(c))(Y(E(o))(V.value))(p(k)(B.create)(i)))(p(k)(nl)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(_){return _.value0})(r)))(p(k)(function(_){return function(v){return function(D){return{e:_,cncl:v,rec:D}}}})(m)))))))([ln(ue)(p(k)(function(_){if(_ instanceof hu)return"Turn on";if(_ instanceof jo)return"Loading...";if(_ instanceof Xo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[_.constructor.name])})(m))]),Be(o)([Db(o)(I(P(c))(Y(E(o))(K(wE)(mb.value)("true")))(I(P(c))(Y(E(o))(K(yv)(Vt.value)("display:none;")))(I(P(c))(p(k)(function(_){return K(OE)(vb.value)(_)})(s))(p(k)(T(K(yv)(Vt.value)("display:block;")))(s)))))([])])])}})))})}}};var cN=function(){return d.value}(),IE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(cN)({periodic:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.2)([DS(EC)(448)(_t())])])}})))})}}};var _N=function(){return d.value}(),RE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(_N)({periodic:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.2)([Wf(Zi)(448)(_t())])])}})))})}}};var sN=function(){return d.value}(),NE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(sN)({periodic:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.2)([C_(Cc)(448)(_t())])])}})))})}}};var vN=function(){return d.value}(),LE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))(o)(vN)({pan:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){return mt(n)([CS(SC)(1)([_e(ge)(a)(_t())])])}})))})}}};var dN=function(){return d.value}(),WE=$t({reflectType:function(){return`<ul>
  <li><a href="#allpass">All-pass filter</a></li>
  <li><a href="#analyser">Analyser</a></li>
  <li><a href="#bandpass">Bandpass filter</a></li>
  <li><a href="#constant">Constant value</a></li>
  <li><a href="#compression">Compression</a></li>
  <li><a href="#convolution">Convolution</a></li>
  <li><a href="#delay">Delay</a></li>
  <li><a href="#gain">Gain</a></li>
  <li><a href="#highpass">Highpass filter</a></li>
  <li><a href="#highshelf">Highshelf filter</a></li>
  <li><a href="#iir">IIR filter</a></li>
  <li><a href="#loopbuf">Looping buffer</a></li>
  <li><a href="#lowpass">Lowpass filter</a></li>
  <li><a href="#lowshelf">Lowshelf filter</a></li>
  <!--<li><a href="#media">Media element</a></li>-->
  <li><a href="#microphone">Microphone</a></li>
  <li><a href="#notch">Notch filter</a></li>
  <!--<li><a href="#panner">Panner</a></li>-->
  <li><a href="#peaking">Peaking filter</a></li>
  <li><a href="#periodic">Periodic wave oscillator</a></li>
  <li><a href="#playbuf">Playing a buffer</a></li>
  <li><a href="#recorder">Recorder</a></li>
  <li><a href="#sawtooth">Sawtooth wave oscillator</a></li>
  <li><a href="#sine">Sine wave oscillator</a></li>
  <li><a href="#square">Square wave oscillator</a></li>
  <li><a href="#pan">Stereo panner</a></li>
  <li><a href="#triangle">Triangle wave oscillator</a></li>
  <li><a href="#waveshaper">Waveshaper</a></li>
    </ul>
`}})()()(Q(c))(o)(dN)({});var AN=function(){return d.value}(),GE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}})(o))(o)(AN)({periodic:N(rt(ht(r)(t)(function(n){return h(ba)(void 0)})(function(n){return function(a){return mt(n)([Pt(it)(.2)([Ss(ms)(448)(_t())])])}})))})}}};var kN=function(){return d.value}(),BE=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(R()(R()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(o)(kN)({code:N(ur(ue)(`do
  let
    makeDistortionCurve :: Number -> Array Number
    makeDistortionCurve k =
      map
        ( \\i ->
            let
              x = (toNumber i * 2.0 / toNumber n_samples) - 1.0
            in
              (3.0 + k) * x * 20.0 * deg / (pi + (k * abs x))
        )
        (0 .. (n_samples - 1))
      where
      n_samples = 44100

      deg = pi / 180.0
  run2_
    [ waveShaper (makeFloatArray (makeDistortionCurve 400.0)) [ loopBuf buf bangOn ] ]`)),waveShaper:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(a){var u=function(f){var i=Qi/180;return p(xe)(function(m){var s=qe(m)*2/qe(44100)-1;return(3+f)*s*20*i/(Qi+f*cm(Na)(_f)(s))})(dn(0)(44099))};return mt(n)([SS(eS)(Zd(u(400)))([_e(ge)(a)(_t())])])}})))})}}};var CN=function(){return d.value}(),UE=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e(Bf.value))(vn),u=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Audio Units</h1>

  <h3>There sure are a lot of them!</h3>
  <p>
    This section provides a tour of the web audio nodes provided by the Web Audio API and, by extension, Wags. There are only two omissions:</p>
    <ul>
      <li>Audio Worklet Nodes</li>
      <li>Multi-channel audio</li>
    </ul>
    <p>Both of these will be covered in later sections.</p>

  <p>
    This section is long and should be read like those passages in the Bible that list who was the child of who: DILIGENTLY AND COPIOUSLY. That said, if you want to skip around, here's a table of contents.
  </p>
  ~toc~
  <p>And now, without further ado... (~drumroll~) Here are some audio nodes!</p>

  ~allpass~
  ~analyser~
  ~bandpass~
  ~constant~
  ~compression~
  ~convolution~
  ~delay~
  ~gain~
  ~highpass~
  ~highshelf~
  ~iirFilter~
  ~loopBuf~
  ~lowpass~
  ~lowshelf~
  ~microphone~
  ~notch~
  ~peaking~
  ~periodicOsc~
  ~playBuf~
  ~recorder~
  ~sawtoothOsc~
  ~sinOsc~
  ~squareOsc~
  ~pan~
  ~triangleOsc~
  ~waveShaper~

  <h2>Next steps</h2>
  <p>Phew, that was a lot of audio units! In the next section, we'll make them come alive thanks to the magic of <a ~next~ style="cursor:pointer;">events</a>.</p>
</div>`}})()()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(_n()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(R()(Q(c))({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}})(o))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}})(o))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}})(o))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}})(o))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}})(o))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}})(o))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}})(o))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}})(o))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}})(o))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}})(o))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}})(o))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}})(o))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}})(o))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}})(o))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}})(o))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}})(o))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}})(o))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}})(o))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}})(o))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}})(o))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}})(o))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}})(o))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}})(o))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}})(o))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}})(o))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}})(o))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}})(o))(o)(CN)({drumroll:N(rt(ws("\u{1F941}")(n)(u)(function(f){return Ct(f)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(f){return function(i){return mt(f)([Pt(it)(1)([_e(ge)(i)(_t())])])}}))),toc:N(WE),allpass:N(tE(u)(e)(n)),analyser:N(_E(u)(e)(n)),bandpass:N(pE(u)(e)(n)),constant:N(dE(u)(e)(n)),compression:N(sE(u)(e)(n)),convolution:N(bE(u)(e)(n)),delay:N(AE(u)(e)(n)),gain:N(yE(u)(e)(n)),highpass:N(kE(u)(e)(n)),highshelf:N(gE(u)(e)(n)),iirFilter:N(CE(u)(e)(n)),loopBuf:N(SE(u)(e)(n)),lowshelf:N(hE(u)(e)(n)),lowpass:N(EE(u)(e)(n)),notch:N(xE(u)(e)(n)),playBuf:N(ME(u)(e)(n)),peaking:N(FE(u)(e)(n)),microphone:N(TE(u)(e)(n)),pan:N(LE(u)(e)(n)),periodicOsc:N($E(u)(e)(n)),recorder:N(PE(u)(e)(n)),sawtoothOsc:N(IE(u)(e)(n)),sinOsc:N(RE(u)(e)(n)),squareOsc:N(NE(u)(e)(n)),triangleOsc:N(GE(u)(e)(n)),waveShaper:N(BE(u)(e)(n)),next:Oa(P(c))(E(o))(n)(a)})}}}};var db=function(){function t(){}return t.value=new t,t}(),qE={attr:function(t){return function(e){return b({key:"checked",value:L(e)})}}};var ho=function(){function t(){}return t.value=new t,t}();var Qo={attr:function(t){return function(e){return b({key:"type",value:L(e)})}}};var To=function(t){return function(e){return function(r){return new U(J(t)("input")(e)(new H(r)))}}};var TN=function(t){return t},Bs=function(t){return function(e){return function(r){return co(t)(I(t.Plus0().Alt0())(Y(t)(e))(r))}}};var W_=function(t){return function(e){return t(e)}},rf=function(t){return{map:function(e){return function(r){return function(n){return r(p(t)(function(a){return function(u){return a(e(u))}})(n))}}}}},Ei=function(t){return function(e){return function(r){return function(n){return W_(p(rf(t.Filterable1().Functor1()))(e)(r))(p(t.Filterable1().Functor1())(jf)(n))}}}};var Jc=function(t){return Ei(t)(T)};var au=TN;var HE=function(t){return function(e){return au(function(r){return br(E(o))(I(P(c))(Y(E(o))(W_(t)(r)))(p(k)(function(n){return W_(n)(r)})(e)))})}},bb=function(t){return{apply:function(e){return function(r){return function(n){return r(e(p(t)(xo(ei))(n)))}}},Functor0:function(){return rf(t)}}};var jc=function(t){return function(e){return Ut(function(r){return Et(e)(function(n){return function(){var u=I_(t)();return r({acTime:u,value:n})()}})})}};var zE=function(t){return function(e){return function(r){var n=function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return function(){var v=wr(f)();return $n(c)(v)(function(){var l=I_(t)(),g=Sp(Tk(wu(Na)(u-l-.04)(.01)*1e3))(function(){var vt=wr(f)();return $n(c)(vt)(function(){return Mn(u)(m)(),a(u)(),n(a)(u+s)(f)(i)(m)(s)()})()})();return Mn(new B(g))(i)()})()}}}}}}};return Ut(function(a){return function(){var f=lr(!0)(),i=lr(V.value)(),m=I_(t)(),s=lr(m+e)();n(a)(e)(f)(i)(s)(e)();var _=Et(r)(function(v){return function(){q(hr)(wr(i))(rr(c)(Je)(Il))();var l=wr(s)();return n(a)(l+v)(f)(i)(s)(v)()}})();return j(tt)(j(tt)(_)(Mn(!1)(f)))(q(hr)(wr(i))(rr(c)(Je)(Il)))}})}}};var Ia=function(t){return function(e){return function(r){return function(n){return function(a){var u=r===t||n===e;if(u)return e;var f=(n-e)/(r-t),i=e-f*t;return f*a+i}}}}};var xN=function(){return d.value}(),VE=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
  <h2>Fold</h2>

  <p>The type of <code>fold</code> is:</p>

  <pre><code>fold
    :: forall event a b
    . IsEvent event
    => (a -> b -> b)
    -> event a
    -> b
    -> event b</code></pre>

  <p>Fold starts with some initial state <code>b</code> and, based on incoming events, allows you to change the state.</p>

  <p>One way <code>fold</code> is useful is to retain when certain actions happen. In the following example, we use <code>requestAnimationFrame</code> to animate the audio and we use four <code>fold</code>-s to store the ambitus and velocity of both vibrato and tremolo.</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p><code>fold</code> is so powerful because it allows us to localize state to <i>any</i> event. In the example above, instead of having a global state, our two folds allow for two <i>ad hoc</i> local states.</p>

</section>`}})()()(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(xN)({txt:N(ur(ue)(`module Main where

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Data.Number (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

type Cbx = V (cbx0 :: Unit, cbx1 :: Unit, cbx2 :: Unit, cbx3 :: Unit)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , cbx :: Cbx
  )

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        stopE = event.startStop.stop
        chkState e = step false $ fold (const not) e false
        cbx0 = chkState event.cbx.cbx0
        cbx1 = chkState event.cbx.cbx1
        cbx2 = chkState event.cbx.cbx2
        cbx3 = chkState event.cbx.cbx3
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                stopE <#> (_ *> push.startStop.start unit)
                startE $> do
                  ctx <- context
                  c0h <- constant0Hack ctx
                  let
                    cevt fast b tm = mapAccum
                      ( \\(oo /\\ act) (pact /\\ pt) ->
                          let
                            tn = pt +
                              ( (act - pact) *
                                  (if oo then fast else 1.0)
                              )
                          in
                            ((act /\\ tn) /\\ tn)
                      )
                      (sampleBy (/\\) b tm)
                      (0.0 /\\ 0.0)

                  r <- run2e ctx
                    ( memoize
                        ( map (add 0.04 <<< _.acTime)
                            $ withACTime ctx animationFrameEvent
                        )
                        \\acTime ->
                          let
                            ev0 = cevt 8.0 cbx0 acTime
                            ev1 = map (if _ then 4.0 else 1.0) $ sample_ cbx1 acTime
                            ev2 = cevt 4.0 cbx2 acTime
                            ev3 = map (if _ then 4.0 else 1.0) $ sample_ cbx3 acTime
                            evs f a = sampleOn acTime
                              $ map ($)
                              $ sampleOn a
                              $ { f: _, a: _, t: _ } <$> f
                          in
                            [ gain 0.0
                                ( evs ev0 ev1 <#> \\{ f, a, t } -> P.gain $ AudioNumeric
                                    { n: calcSlope 1.0 0.01 4.0 0.15 a * sin (pi * f) + 0.15
                                    , o: t
                                    , t: _linear
                                    }
                                )
                                [ periodicOsc
                                    { frequency: 325.6
                                    , spec: (0.3 +> -0.1 +> 0.7 +> -0.4 +> V.empty)
                                        /\\ (0.6 +> 0.3 +> 0.2 +> 0.0 +> V.empty)
                                    }
                                    ( OneOf.do
                                        bangOn
                                        evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                          $ AudioNumeric
                                              { n: 325.6 +
                                                  (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                              , o: t
                                              , t: _linear
                                              }
                                    )
                                ]
                            ]
                    )
                  push.startStop.stop (r *> c0h *> close ctx)
            )
            [ text OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        , D.div
            ( O.oneOfMap (map (attr D.Style)) O.do
                stopE $> "display:block;"
                startE $> "display:none;"
            )
            ( map
                ( \\e -> D.input
                    ( OneOf.do
                        bang (D.Xtype := "checkbox")
                        bang (D.OnClick := cb (const (e unit)))
                        startE $> (D.Checked := "false")
                    )
                    []
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:N(rt(du()(o)(Nu({reflectSymbol:function(){return"cbx"}})()()()(ar({reflectSymbol:function(){return"cbx0"}})()()(ar({reflectSymbol:function(){return"cbx1"}})()()(ar({reflectSymbol:function(){return"cbx2"}})()()(ar({reflectSymbol:function(){return"cbx3"}})()()(Hn)()()()())()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(Hn)()()()())()()()())(d.value)(function(a){return function(u){var f=I(P(c))(Y(E(o))(void 0))(u.startStop.start),i=function(D){return Bs(E(o))(!1)(Iu(E(o))(T(pu(Ga)))(D)(!1))},m=i(u.cbx.cbx3),s=i(u.cbx.cbx2),_=i(u.cbx.cbx1),v=i(u.cbx.cbx0);return Be(o)([Tn(o)(Qr(Xt)(C(c))(p(k)(function(){var D=K(pr)(_r.value);return function(l){return D(Xe(T(l)))}}()))([Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(D){return D.value0})(n)))(X(k)(f)(Z(et))))(function(D){return function(){D();var g=oa(or)(),ot=Ju(or)(g)(),vt=function(he){return function(gr){return function(lt){return Rl(E(o))(function(oe){return function(ke){var Gt=ke.value1+(oe.value1-ke.value0)*function(){return oe.value0?he:1}();return new nt(new nt(oe.value1,Gt),Gt)}})(Ei(E(o))(nt.create)(gr)(lt))(new nt(0,0))}}},Wt=Bc(g)(Vi(o)(p(k)(function(){var he=Le(ga)(.04);return function(gr){return he(function(lt){return lt.acTime}(gr))}}())(jc(g)(ef)))(function(he){var gr=function(mr){return function(Tu){return co(E(o))(he)(p(k)(nl)(co(E(o))(Tu)(p(k)(function(ju){return function(Mi){return function(ou){return{f:ju,a:Mi,t:ou}}}})(mr))))}},lt=p(k)(function(mr){return mr?4:1})(Jc(E(o))(m)(he)),oe=vt(4)(s)(he),ke=p(k)(function(mr){return mr?4:1})(Jc(E(o))(_)(he)),Gt=vt(8)(v)(he);return[qr(it)(0)(Ze(k)(gr(Gt)(ke))(function(mr){return Fn()(Nr)({n:Ia(1)(.01)(4)(.15)(mr.a)*Pp(Qi*mr.f)+.15,o:mr.t,t:Ho})}))([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:325.6,spec:new nt(we(Me(ua)()(An)()(Ha))(.3)(we(Me(ku)()(yn)()(An))(-.1)(we(Me(gu)()(kn)()(yn))(.7)(we(Me(Cu)()(Su)()(kn))(-.4)(Hu)))),we(Me(ua)()(An)()(Ha))(.6)(we(Me(ku)()(yn)()(An))(.3)(we(Me(gu)()(kn)()(yn))(.2)(we(Me(Cu)()(Su)()(kn))(0)(Hu)))))})(dr(Xt)(C(c))([_t(),Ze(k)(gr(oe)(lt))(function(mr){return Eo()(Nr)({n:325.6+Ia(1)(3)(4)(15.5)(mr.a)*Pp(Qi*mr.f),o:mr.t,t:Ho})})]))])]}))(),re=j(tt)(j(tt)(Wt)(ot))(xn(or)(g));return t(j(tt)(re)(a.startStop.start(void 0)))(),a.startStop.stop(re)()}}),Ze(k)(u.startStop.stop)(function(D){return j(tt)(D)(j(tt)(t(h(c)(void 0)))(a.startStop.start(void 0)))})]))([ln(ue)(dr(Xt)(C(c))([X(k)(f)("Turn on"),X(k)(u.startStop.stop)("Turn off")]))]),Te(o)(Qr(Xt)(C(c))(p(k)(K(pt)(Vt.value)))([X(k)(u.startStop.stop)("display:block;"),X(k)(f)("display:none;")]))(p(xe)(function(D){return To(o)(dr(Xt)(C(c))([Y(E(o))(K(Qo)(ho.value)("checkbox")),Y(E(o))(K(pr)(_r.value)(Xe(T(D(void 0))))),X(k)(f)(K(qE)(db.value)("false"))]))([])})(em(xe)([function(D){return D.cbx0},function(D){return D.cbx1},function(D){return D.cbx2},function(D){return D.cbx3}])(a.cbx)))])}})))})}}}};var Ab={recip:function(t){return 1/t},Ring0:function(){return _f}};var yb=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function Xc(t){return function(){return function(e){return t(e)()}}}function Qc(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function Kc(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function kb(t){return t.clientX}function gb(t){return t.clientY}function G_(t){return t.button}var B_=Rt("MouseEvent");var JE=function(t){return function(e){return Ut(function(r){return Et(e)(function(n){return function(){var u=wr(t.buttons)();return r({value:n,buttons:u})()}})})}};var jE=function(){var e=lr(V.value)(),r=lr(tv)(),n=p(x)(sb)(Si)(),a=Xc(function(m){return rr(c)(Je)(function(s){return Mn(new B({x:kb(s),y:gb(s)}))(e)})(B_(m))})(),u=Xc(function(m){return rr(c)(Je)(function(s){return Kf(Gy(Jr)(G_(s)))(r)})(B_(m))})(),f=Xc(function(m){return rr(c)(Je)(function(s){return Kf(Cp(Jr)(G_(s)))(r)})(B_(m))})();Qc(fn()("mousemove"))(a)(!1)(n)(),Qc(fn()("mousedown"))(u)(!1)(n)(),Qc(fn()("mouseup"))(f)(!1)(n)();var i=function(){return Kc(fn()("mousemove"))(a)(!1)(n)(),Kc(fn()("mousedown"))(u)(!1)(n)(),Kc(fn()("mouseup"))(f)(!1)(n)()};return{position:e,buttons:r,dispose:i}},XE=Ut(function(t){return function(){var r=p(x)(sb)(Si)(),n=Xc(function(a){return rr(c)(Je)(function(u){return t(G_(u))})(B_(a))})();return Qc(fn()("mousedown"))(n)(!1)(r)(),Kc(fn()("mousedown"))(n)(!1)(r)}});var KE=function(t){return au(function(e){return p(k)(function(r){return r.value(r.buttons)})(JE(t)(e))})};var Eb=function(t){return t};function Hs(){return Date.now()}var gh=function(t){return Ut(function(e){return Et(t)(function(r){return function(){var a=Hs();return e({time:a,value:r})()}})})};var l1=au(function(t){return p(k)(function(e){return e.value(e.time)})(gh(t))}),Tb=p(rf(k))(function(){var t=T_(LS);return function(e){return t(Eb(e))}}())(l1);var p1=function(t){var e=function(u){return function(f){return function(i){return function(m){return function(s){return function(_){return function(v){var D=Le(f.DivisionRing1().Ring0().Semiring0())(Ca(f.DivisionRing1().Ring0().Semiring0()))(Ca(f.DivisionRing1().Ring0().Semiring0())),l=function(g){return function(ot){if(g.last instanceof V)return ot;if(g.last instanceof B)return Le(i)(ot)(m(function(vt){return Zu(f.EuclideanRing0())(Wn(f.DivisionRing1().Ring0().Semiring0())(vt(Le(i)(g.last.value0.value1)(g.now.value1)))(Mu(f.DivisionRing1().Ring0())(g.now.value0)(g.last.value0.value0)))(D)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[g.constructor.name,ot.constructor.name])}};return au(function(g){var ot=W_(v)(X(u.Filterable1().Functor1())(g)(Z(et))),vt=hp(u)(Ei(u)(nt.create)(_)(ot)),Wt=Iu(u)(l)(vt)(s);return co(u)(Wt)(g)})}}}}}}},r=function(u){return function(f){return e(u)(f)(f.DivisionRing1().Ring0().Semiring0())(function(i){return i(Z(et))})}},n=function(u){return function(f){return au(function(i){return Nl(E(o))(function(m){var s=f(Bs(E(o))(u)(m));return{input:Jc(E(o))(s)(i),output:co(E(o))(m)(i)}})})}},a=function(u){return function(f){return function(i){if(Wy(u))return-8*(f-1)-i*2;if(Ye)return 2*(4-f);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[u.constructor.name,f.constructor.name,i.constructor.name])}}};return n(2)(function(u){return r(E(o))(yb(pl)(Ab))(2)(p(rf(k))(wn())(Tb))(function(){var f=n(10)(function(i){return r(E(o))(yb(pl)(Ab))(10)(p(rf(k))(wn())(Tb))(Ht(bb(k))(Ht(bb(k))(p(rf(k))(a)(KE(t)))(u))(i))});return HE(f)(X(k)(XE)(f))}())})},s1=function(){return d.value}(),Ch=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}})(o))(o)(s1)({txt:N(ur(ue)(`module Main

import Prelude

import Control.Alt ((<|>))
import QualifiedDo.OneOfMap as O
import QualifiedDo.Alt as OneOf
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (second)
import Data.Set (isEmpty)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb)
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random (randomInt)
import FRP.Behavior (ABehavior, Behavior, behavior, sample, sampleBy, sample_, step, switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Time as Time
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (class IsEvent, bang, fix, fold, sampleOn, withLast)
import FRP.Event.Mouse (Mouse, down, getMouse)
import FRP.Event.VBus (V, vbus)
import Test.QuickCheck (arbitrary, mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (bandpass_, gain, lowpass_, periodicOsc, squareOsc_)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Core (AudioNumeric(..), _linear, bangOn)
import WAGS.Properties as P
import WAGS.Run (run2e)

type StartStop = V (start :: Unit, stop :: Effect Unit)

-- \`swell\` is an interactive function of time defined by a differential equation:
--
-- d^2s/dt^2
--   | mouse down = \u237A - \u03B2s
--   | mouse up   = \u0263 - \u03B4s - \u03B5 ds/dt
--
-- So the function exhibits either decay or growth depending on if
-- the mouse is pressed or not.
--
-- We can solve the differential equation by integration using \`solve2'\`.
swell :: Mouse -> Behavior Number
swell mouse =
  fixB 2.0 \\b ->
    integral' 2.0 (unwrap <$> Time.seconds)
      let
        db = fixB 10.0 \\db_ ->
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
      in
        switcher db (down $> db)
  where
  f bs s ds
    | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
    | otherwise = 2.0 * (4.0 - s)

  fixB :: forall a. a -> (Behavior a -> Behavior a) -> Behavior a
  fixB a fn = behavior \\s ->
    fix \\event ->
      let
        b = fn (step a event)
      in
        { input: sample_ b s, output: sampleOn event s }

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function approximates the integral using the trapezium rule at the
  -- | implicit sampling interval.
  -- |
  -- | The \`Semiring\` \`a\` should be a vector field over the field \`t\`. To represent
  -- | this, the user should provide a _grate_ which lifts a multiplication
  -- | function on \`t\` to a function on \`a\`. Simple examples where \`t ~ a\` can use
  -- | the \`integral'\` function instead.
  integral
    :: forall event a t
     . IsEvent event
    => Field t
    => Semiring a
    => (((a -> t) -> t) -> a)
    -> a
    -> ABehavior event t
    -> ABehavior event a
    -> ABehavior event a
  integral g initial t b =
    behavior \\e ->
      let
        x = sample b (e $> identity)
        y = withLast (sampleBy (/\\) t x)
        z = fold approx y initial
      in
        sampleOn z e
    where
    approx { last: Nothing } s = s
    approx { now: t1 /\\ a1, last: Just (t0 /\\ a0) } s = s + g (\\z -> z (a0 + a1) * (t1 - t0) / two)

    two :: t
    two = one + one

  -- | Integrate with respect to some measure of time.
  -- |
  -- | This function is a simpler version of \`integral\` where the function being
  -- | integrated takes values in the same field used to represent time.
  integral'
    :: forall event t
     . IsEvent event
    => Field t
    => t
    -> ABehavior event t
    -> ABehavior event t
    -> ABehavior event t
  integral' = integral (_ $ identity)

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ StartStop) \\push event -> do
      let
        startE = bang unit <|> event.start
        stopE = event.stop
      D.div_
        [ D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                event.stop <#> (_ *> push.start unit)
                startE $>
                  do
                    ctx <- context
                    c0h <- constant0Hack ctx
                    mouse <- getMouse
                    ri <- randomInt 0 10000
                    let
                      ttap (o /\\ n) = AudioNumeric { o: o + 0.04, n, t: _linear }
                      fund = 90.4
                      spcn = map (_ - 0.5) arbitrary
                      spc' = do
                        a <- spcn
                        b <- spcn
                        c <- spcn
                        d <- spcn
                        pure (a +> b +> c +> d +> V.empty)
                      spc = (/\\) <$> spc' <*> spc'
                      spcs = { s0: _, s1: _, s2: _, s3: _ } <$> spc <*> spc <*> spc <*> spc
                      allSpcs = evalGen spcs { newSeed: mkSeed ri, size: 5 }
                    r <- run2e ctx
                      ( memoize
                          ( map (\\{ acTime, value } -> acTime /\\ value)
                              $ withACTime ctx
                              $ sample_ (swell mouse) animationFrameEvent
                          )
                          \\swm ->
                            [ gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.4) $ 0.5 * (x - 1.0)) <$> swm
                                )
                                [ lowpass_ { frequency: fund, q: 20.0 }
                                    [ squareOsc_ fund ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.2) $ 0.4 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 4.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: (fund * 3.02)
                                        , spec: allSpcs.s0
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 3.02 + 14.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max (-0.1) $ 0.2 * (x - 6.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 6.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 5.07
                                        , spec: allSpcs.s1
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 5.07 + 18.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.2 * (x - 3.0)) <$> swm
                                )
                                [ bandpass_ { frequency: fund * 8.0, q: 20.0 }
                                    [ periodicOsc
                                        { frequency: fund * 7.13
                                        , spec: allSpcs.s2
                                        }
                                        ( bangOn <|>
                                            ( P.frequency
                                                <<< ttap
                                                <<< second (\\x -> fund * 7.13 + 32.0 * (x - 1.0)) <$> swm
                                            )
                                        )
                                    ]
                                ]
                            , gain 0.0
                                ( P.gain
                                    <<< ttap
                                    <<< second (\\x -> max 0.0 $ 0.1 * (x - 7.0)) <$> swm
                                )
                                [ periodicOsc
                                    { frequency: fund * 9.14
                                    , spec: allSpcs.s3
                                    }
                                    ( bangOn <|>
                                        ( P.frequency
                                            <<< ttap
                                            <<< second (\\x -> fund * 9.14 + 31.0 * (x - 1.0)) <$> swm
                                        )
                                    )
                                ]
                            ]
                      )
                    push.stop (r *> c0h *> close ctx)
            )
            [ text $ OneOf.do
                startE $> "Turn on"
                stopE $> "Turn off"
            ]
        ]
  )`)),empl:N(rt(du()(o)(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(d.value)(function(a){return function(u){var f=I(P(c))(Y(E(o))(void 0))(u.start);return Be(o)([Tn(o)(Qr(Xt)(C(c))(p(k)(function(){var i=K(pr)(_r.value);return function(m){return i(Xe(T(m)))}}()))([Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(n)))(X(k)(f)(Z(et))))(function(i){return function(){i();var s=oa(or)(),_=Ju(or)(s)(),v=jE(),D=Jl(0)(1e4)(),l=function(lt){return{o:lt.value0+.04,n:lt.value1,t:Ho}},g=p(po)(function(lt){return lt-.5})(u_(ng)),ot=q(Ff)(g)(function(lt){return q(Ff)(g)(function(oe){return q(Ff)(g)(function(ke){return q(Ff)(g)(function(Gt){return h(n_)(we(Me(ua)()(An)()(Ha))(lt)(we(Me(ku)()(yn)()(An))(oe)(we(Me(gu)()(kn)()(yn))(ke)(we(Me(Cu)()(Su)()(kn))(Gt)(Hu)))))})})})}),vt=Ht($f)(p(po)(nt.create)(ot))(ot),Wt=Ht($f)(Ht($f)(Ht($f)(p(po)(function(lt){return function(oe){return function(ke){return function(Gt){return{s0:lt,s1:oe,s2:ke,s3:Gt}}}}})(vt))(vt))(vt))(vt),re=bc(Wt)({newSeed:mc(D),size:5}),he=Bc(s)(Vi(o)(p(k)(function(lt){return new nt(lt.acTime,lt.value)})(jc(s)(Jc(E(o))(p1(v))(ef))))(function(lt){return[qr(it)(0)(p(k)(function(){var oe=Fn()(Nr),ke=Xn(gn)(function(Gt){return wu(Na)(-.4)(.5*(Gt-1))});return function(Gt){return oe(l(ke(Gt)))}}())(lt))([Oc(ID(kt(yt()(z(z(At)(OC)()()()({reflectSymbol:function(){return"q"}}))(CD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4,q:20})([bS(Cc)(90.4)])]),qr(it)(0)(p(k)(function(){var oe=Fn()(Nr),ke=Xn(gn)(function(Gt){return wu(Na)(-.2)(.4*(Gt-3))});return function(Gt){return oe(l(ke(Gt)))}}())(lt))([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*4,q:20})([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*3.02,spec:re.s0})(I(P(c))(_t())(p(k)(function(){var oe=Eo()(Nr),ke=Xn(gn)(function(Gt){return 90.4*3.02+14*(Gt-1)});return function(Gt){return oe(l(ke(Gt)))}}())(lt)))])]),qr(it)(0)(p(k)(function(){var oe=Fn()(Nr),ke=Xn(gn)(function(Gt){return wu(Na)(-.1)(.2*(Gt-6))});return function(Gt){return oe(l(ke(Gt)))}}())(lt))([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*6,q:20})([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*5.07,spec:re.s1})(I(P(c))(_t())(p(k)(function(){var oe=Eo()(Nr),ke=Xn(gn)(function(Gt){return 90.4*5.07+18*(Gt-1)});return function(Gt){return oe(l(ke(Gt)))}}())(lt)))])]),qr(it)(0)(p(k)(function(){var oe=Fn()(Nr),ke=Xn(gn)(function(Gt){return wu(Na)(0)(.2*(Gt-3))});return function(Gt){return oe(l(ke(Gt)))}}())(lt))([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*8,q:20})([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*7.13,spec:re.s2})(I(P(c))(_t())(p(k)(function(){var oe=Eo()(Nr),ke=Xn(gn)(function(Gt){return 90.4*7.13+32*(Gt-1)});return function(Gt){return oe(l(ke(Gt)))}}())(lt)))])]),qr(it)(0)(p(k)(function(){var oe=Fn()(Nr),ke=Xn(gn)(function(Gt){return wu(Na)(0)(.1*(Gt-7))});return function(Gt){return oe(l(ke(Gt)))}}())(lt))([Ai(bi(kt(yt()(z(z(At)(di(vi(ua)))()()()({reflectSymbol:function(){return"spec"}}))(Di)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*9.14,spec:re.s3})(I(P(c))(_t())(p(k)(function(){var oe=Eo()(Nr),ke=Xn(gn)(function(Gt){return 90.4*9.14+31*(Gt-1)});return function(Gt){return oe(l(ke(Gt)))}}())(lt)))])]}))(),gr=j(tt)(j(tt)(he)(_))(xn(or)(s));return t(j(tt)(gr)(a.start(void 0)))(),a.stop(gr)()}}),Ze(k)(u.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(a.start(void 0)))})]))([ln(ue)(dr(Xt)(C(c))([X(k)(f)("Turn on"),X(k)(u.stop)("Turn off")]))])])}})))})}}}};var v1=function(){return d.value}(),Sh=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(R()(R()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}})(o))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}})(o))(o)(v1)({next:Oa(P(c))(E(o))(n)(j(tt)(e(O_.value))(vn)),fold:N(VE(a)(e)(r)(n)),fix:N(Ch(a)(e)(r)(n))})}}}};var d1=function(){function t(){}return t.value=new t,t}(),Eh=function(){function t(){}return t.value=new t,t}(),xb=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),b1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((..))
import Data.Int (toNumber)
import Data.Profunctor (lcmap)
import Deku.Attribute (cb, (:=))
import Deku.Control (text)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import FRP.Event (bus)
import FRP.Event.Class (bang)
import Math (pow)
import WAGS.Control (gain_, gain, sinOsc)
import WAGS.Core (AudioEnvelope(..), AudioOnOff(..), _on, _off)
import WAGS.Properties (onOff)
import WAGS.Properties as P
import WAGS.Run (run2_)

data UIEvents = Init | Start | Stop (Effect Unit)

-- an event to turn our oscillators on
oon o = bang $ onOff $ AudioOnOff { x: _on, o }
-- an event to turn our oscillators off
oof o = bang $ onOff $ AudioOnOff { x: _off, o }
-- an event with an envelope for our gain
env o = bang $ P.gain
  $ AudioEnvelope
      { p: [ 0.0, 0.4, 0.1, 0.05, 0.01, 0.0 ]
      , d: 0.8
      , o
      }

-- a single cell with four oscillators,
-- each of which have the envelope applied
cell = lcmap toNumber \\i -> do
  let
    ooo' x = oon (x + 0.27 * (i * (1.005 \`pow\` i)))
      <|> oof (x + 3.0 + 0.3 * (i * (1.005 \`pow\` i)))
    env' x = env (x + 0.3 * (i * (1.005 \`pow\` i)))
    strand x y =
      gain 0.0 (env' x) [ sinOsc (200.0 + i * y) (ooo' x) ]
  [ strand 0.2 4.0
  , strand 0.3 6.0
  , strand 0.45 14.0
  , strand 0.7 20.0
  ]

main :: Effect Unit
main = runInBody1 (bus \\push -> lcmap (bang Init <|> _) \\event ->
  D.div_
    [ D.button
        ( event <#>
            \\e -> D.OnClick := cb
              ( const $ case e of
                  Stop u -> u *> push Start
                  _ -> do
                    r <- run2_
                      [ gain_ 1.0
                          -- we create 100 cells
                          $ join
                          $ cell <$> 0 .. 100
                      ]
                    push $ Stop r
              )
        )
        [ text $ event <#> case _ of
            Stop _ -> "Turn off"
            _ -> "Turn on"
        ]
    ])
`;var A1=function(){return d.value}(),y1=function(t){return function(e){return function(r){return Y(t)(Hf(e)(Pf)({x:kD,o:r}))}}},k1=function(t){return function(e){return function(r){return Y(t)(Hf(e)(Pf)({x:kC,o:r}))}}},g1=fo(xa)(qe)(function(t){var e=function(a){return I(P(c))(y1(E(o))()(a+.27*(t*Xi(1.005)(t))))(k1(E(o))()(a+3+.3*(t*Xi(1.005)(t))))},r=function(a){return Y(E(o))(Fn()(Rn)({p:[0,.4,.1,.05,.01,0],o:a+.3*(t*Xi(1.005)(t)),d:.8}))},n=function(a){return function(u){return qr(it)(0)(r(a))([Wf(Zi)(200+t*u)(e(a))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),hh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Example 1: Hello events</h2>

  <p>Let's say hi to events! The simplest of events, which we've seen already, are the ones that occur <span style="font-weight:800;">now</span>, that is to say, immediately upon subscription. You create those types of events using <code>bang</code>. In this section, we'll use <code>bang</code> to set several different types of values:</p>

  <ul>
    <li><code>AudioEnvelope</code> to create an envelope for the gain node. To construct one, use a record with the following parameters:<ul><li><code>p</code>: a list of numbers that will be interpolated over.</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li><li><code>d</code>: the duration of the envelope.</li></ul></li>
    <li><code>AudioOnOff</code> to turn the sine-wave oscillator on and off. To construct one, use a record with the following parameters:<ul><li><code>n</code>: an enum with the value <code>_on</code>, <code>_off</code> or <code>_onOff</code> (more on this in <a href="#example3">Example 3</a> below).</li><li><code>o</code>: the offset in time from the AudioContext clock's start time.</li></ul></li>
  </ul>

  <p>After that, in the example below, it's functions all the way down. <code>oon</code> and <code>oof</code> create our on/off events, <code>env</code> creates our gain envelope, <code>ooo'</code> and <code>env'</code> specialize these envelopes to a specific point in time, and <code>cell</code> creates a single cell that we deploy 100 times.</p>

  <p>One important thing to note here is the use of the tie fighter (<code>&lt;|&gt;</code>), aka <code>alt</code>, in the definition of <code>ooo'</code>. The <code>Event</code> type, when <code>alt</code>'d, preserves a before-after relationship of the left and right operands when the operands happen at the same time. This is a bit hackish: the events conceptually happen at the same time, but on our CPU, one has to follow the other. We can use this, however, to make sure that certain events happen in a logical sequence. For example, an <code>off</code> instruction must be issued after an <code>on</code> instruction, which we guarantee by using <code>oon</code> on the left side of the alt. If we did it the other way, the <code>on</code> instruction would be last and we'd wind up with 100 oscillators playing at the same time!</p>

  <p>A last thing to note before the music plays is how scheduling works here. Even though all the events are issued upfront via <code>bang</code>, they schedule things to be played <i>later</i> in the audio context. We'll see more advanced scheduling techniques in the <a href="#example4"><code>requestAnimationFrame</code> example below</a>.</p>

  <pre><code>@txt@</code></pre>

  @ex0@

  <p>Unlike the previous examples, this one and all subsequent ones are "batteries included", meaning they are single-file, self-contained PureScript examples that you can compile and run yourself.</p>

</section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)(A1)({txt:N(ur(ue)(b1)),ex0:N(rt(Zr(o)(function(n){return fo(xa)(function(a){return I(P(c))(Y(E(o))(d1.value))(a)})(function(a){return Be(o)([Tn(o)(Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(u){return u.value0})(r)))(p(k)(nt.create)(a)))(function(u){return K(pr)(_r.value)(Xe(T(function(){return u.value0 instanceof xb?j(tt)(j(tt)(u.value0.value0)(n(Eh.value)))(t(h(c)(void 0))):function(){u.value1();var i=Os([Pt(it)(1)(Zn(ri)(p(xe)(g1)(dn(0)(100))))])();return t(j(tt)(i)(n(Eh.value)))(),n(new xb(i))()}}())))}))([ln(ue)(Ze(k)(a)(function(u){return u instanceof xb?"Turn off":"Turn on"}))])])})})))})}}};var hi=function(){function t(){}return t.value=new t,t}();var af={attr:function(t){return function(e){return b({key:"max",value:L(e)})}}};var Ti=function(){function t(){}return t.value=new t,t}();var uf={attr:function(t){return function(e){return b({key:"min",value:L(e)})}}};var xi=function(){function t(){}return t.value=new t,t}();var of={attr:function(t){return function(e){return b({key:"input",value:ct(e)})}}};var Fi=function(){function t(){}return t.value=new t,t}(),ff={attr:function(t){return function(e){return b({key:"step",value:L(e)})}}};var $i=function(){function t(){}return t.value=new t,t}();var cf={attr:function(t){return function(e){return b({key:"value",value:L(e)})}}};var Ko=function(t){return function(e){return function(r){return I(t)(e)(r(void 0))}}};var S1=Rk,uu={convert:function(t){return t}},U_={convert:function(t){return Ql(t)}},xh=function(t){return t},Fb=function(t){return t.convert},Ja=function(t){return function(e){return function(r){return dt(S1)(Ql(e))(Fb(t)(r(void 0)))}}};var q_=function(t){return function(e){return function(r){return function(n){return Qr(Nk)(e)(r)(xh(Fb(t)(n)))}}}};function $h(t){return t.target}var Yc=function(t){return tn($h(t))};var T1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, toDOM)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.Alt as OneOf
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Core (bangOn)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Properties (loopEnd, loopStart, playbackRate)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type Slider = V (s0 :: Number, s1 :: Number, s2 :: Number)
type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Slider)

atari =
  "https://freesound.org/data/previews/100/100981_1234256-lq.mp3" :: String

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx atari >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ toDOM $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        sl0 = event.slider.s0
        sl1 = event.slider.s1
        sl2 = event.slider.s2
        start = event.startStop.start <|> bang unit
        music = run2_
          [ loopBuf
              { buffer: buffer
              , playbackRate: 2.6
              , loopStart: 0.6
              , loopEnd: 1.1
              }
              OneOf.do
                bangOn
                map
                  (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                  sl0
                map
                  (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                  sl1
                map
                  (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                  (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
          ]
      D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( O.oneOfMap bang O.do
                        D.Xtype := "range"
                        D.Min := "0"
                        D.Max := "100"
                        D.Step := "1"
                        D.Value := "50"
                        D.OnInput := cb
                          ( traverse_
                              (valueAsNumber >=> f)
                              <<< (=<<) fromEventTarget
                              <<< target
                          )
                    )
                    []
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                    start $> (music >>= push.startStop.stop)
                    event.startStop.stop <#>
                      (_ *> push.startStop.start unit)
                )
                [ text OneOf.do
                    start $> "Turn on"
                    event.startStop.stop $> "Turn off"
                ]
            ]
`,x1=function(){return d.value}(),F1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",Mh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
 <h2>Example 2: Three sliders</h2>

  <p>In this example, we'll use three sliders to control the playback rate, the start time, and the end time of a looping buffer.</p>

  <p>There is a fair bit of DOM-related code in this example, so before showing the whole thing, let's isolate the Wags bit.</p>

  <pre><code>@wagtxt@</code></pre>

  <p>Note that our loopBuf consumes four events: in addition to the three sliders, there is a <code>bangOn</code> event that turns it on. For the events belonging to range sliders, we use <code>calcSlope</code> to normalize the range to sensible values for these parameters.</p>

  <p>Because each slider event contains a number, we can compose it with a function from <code>WAGS.Properties</code>, like <code>playbackRate</code> or <code>loopStart</code>, to create an event that controls a Wags parameter. The <code>oneOf</code> directive indicates that the incoming event will be "one of" the events in the array. It's also possible to use the tie-fighter, aka <code>alt</code>, to separate each event, but I like the array syntax when possible as tie fighters do, after all, work for the Empire, and who likes the Empire?</p>

  <p>And below you'll find the full example. It also shows useful patterns like downloading audio files and filtering events.</p>

  <pre><code>@txt@</code></pre>

  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(R()(Q(c))({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}})(o))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(x1)({wagtxt:N(ur(ue)(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ OneOf.do
      bangOn
      (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))`)),txt:N(ur(ue)(T1)),ex1:N(rt(du()(o)(Nu({reflectSymbol:function(){return"slider"}})()()()(ar({reflectSymbol:function(){return"s0"}})()()(ar({reflectSymbol:function(){return"s1"}})()()(ar({reflectSymbol:function(){return"s2"}})()()(Hn)()()()())()()()())()()()())(Nu({reflectSymbol:function(){return"startStop"}})()()()(ar({reflectSymbol:function(){return"loading"}})()()(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=I(P(c))(a.startStop.start)(Y(E(o))(void 0)),f=function(i){return _e(Nf(kt(yt()(z(z(z(z(At)(Fc)()()()({reflectSymbol:function(){return"playbackRate"}}))(A_)()()()({reflectSymbol:function(){return"loopStart"}}))(b_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Rf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Ko(P(c))(_t())(function(){return Ko(P(c))(p(k)(function(){var m=ia()(ss),s=Ia(0)(.2)(100)(5);return function(_){return m(s(_))}}())(a.slider.s0))(function(){return Ko(P(c))(p(k)(function(){var m=vE(),s=Ia(0)(0)(100)(1.2);return function(_){return m(s(_))}}())(a.slider.s1))(function(){return p(k)(function(){var m=DE(),s=Ia(0)(.05)(100)(1);return function(_){return m(s(_))}}())(Pn(E(o))(a.slider.s2)(p(k)(Le(ga))(I(P(c))(Y(E(o))(0))(a.slider.s1))))})})}))};return Be(o)(dt(ya)(p(xe)(function(i){return Be(o)([ur(ue)(i.l),To(o)(q_(uu)(C(c))(Y(E(o)))(Ja(uu)(K(Qo)(ho.value)("range"))(function(){return Ja(uu)(K(uf)(Ti.value)("0"))(function(){return Ja(uu)(K(af)(hi.value)("100"))(function(){return Ja(uu)(K(ff)(Fi.value)("1"))(function(){return Ja(U_)(K(cf)($i.value)("50"))(function(){return K(of)(xi.value)(Xe(function(){var m=rr(c)(Je)(Xf(hr)(qf)(i.f)),s=Yn(Ea)(zc);return function(_){return m(s(Yc(_)))}}()))})})})})})))([])])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([Tn(o)(q_(uu)(C(c))(p(k)(function(){var i=K(pr)(_r.value);return function(m){return i(Xe(T(m)))}}()))(Ja(uu)(X(k)(a.startStop.loading)(h(c)(void 0)))(function(){return Ja(U_)(Ze(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}))(function(){return Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=So(q(Rr)(oa(Ir))(function(_){return q(Rr)(Ju(Ir)(_))(function(v){return q(Rr)(Ct(_)(F1))(function(D){return sr(Ir)(function(){var g=mt(_)([f(D)])(),ot=j(tt)(j(tt)(g)(v))(xn(or)(_));return n.startStop.stop(ot)(),ot})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(gi(s))()})(),void 0}})})})))([ln(ue)(Ko(P(c))(p(k)(T("Turn off"))(a.startStop.stop))(function(){return p(k)(T("Turn on"))(u)}))])]))}})))})}}};var M1=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import QualifiedDo.OneOfMap as O
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (text, text_)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (Event, makeEvent, memoize, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Clock (interval)
import WAGS.Control (bandpass_, fan1, gain, gain_, highpass_, triangleOsc)
import WAGS.Core (Audible, AudioEnvelope(AudioEnvelope), bangOn)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Properties (frequency)
import WAGS.Properties as P
import WAGS.Run (run2e)
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, valueAsNumber)

type StartStop = V (start :: Unit, stop :: Effect Unit)

type UIEvents = V
  ( startStop :: StartStop
  , slider :: Number
  )

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

-- pentatonic scale
cp :: Number -> Number
cp n
  | n < 0.142857 = 261.625565
  | n < 0.285714 = 293.664768
  | n < 0.428571 = 349.228231
  | n < 0.571429 = 391.995436
  | n < 0.714286 = 440.000000
  | n < 0.857143 = 523.251131
  | otherwise = 587.329536

main :: Effect Unit
main = runInBody1
  ( vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        start = event.startStop.start <|> bang unit

        music :: forall lock. _ -> Event (Array (Audible _ lock _))
        music evt' = memoize evt' \\evt -> do
          let
            pitch = map fst evt
            -- to avoid artifacts in the pitch change
            time = map (add 0.01 <<< snd) evt
            e0 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.6, 0.2, 0.1, 0.5, 0.03, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e1 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.3, 0.1, 0.05, 0.01, 0.005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            e2 =
              AudioEnvelope <<<
                { p: [ 0.0, 0.15, 0.05, 0.01, 0.005, 0.0005, 0.0 ]
                , d: 0.4
                , o: _
                } <$> time
            f0 = bangOn <|> frequency <<< cp <$> pitch
          [ fan1 (triangleOsc 0.0 f0) \\ipt _ -> do
              gain_ 2.0
                [ gain 0.0 (P.gain <$> e0)
                    [ bandpass_
                        { frequency: 1000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e1)
                    [ bandpass_
                        { frequency: 2000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                , gain 0.0 (P.gain <$> e2)
                    [ highpass_
                        { frequency: 4000.0
                        , q: 20.0
                        }
                        [ ipt ]
                    ]
                ]
          ]
      D.div_
        [ D.div_
            [ text_ "tempo"
            , D.input
                ( O.oneOfMap bang O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb
                      ( traverse_
                          (valueAsNumber >=> push.slider)
                          <<< (=<<) fromEventTarget
                          <<< target
                      )
                )
                []
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ start $> do
                    ctx <- context
                    let
                      myIvl = sampleBy Tuple random
                        $ interval ctx 0.91
                        $ map (calcSlope 0.0 0.42 100.0 1.4)
                        $ event.slider
                    r <- run2e ctx (music myIvl)
                    push.startStop.stop (r *> close ctx)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ start $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
  )`,w1=au(function(t){return Ut(function(e){return Et(t)(function(r){return function(){var a=Uu();return e(r(a))()}})})}),O1=function(){return d.value}(),P1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(Ye)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},wh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Example 3: Fascinating rhyhtm</h2>

  <p>Wags comes with several different ways to hook into the Web Audio API's sample-accurate timers. In this section, we'll use a Wags <code>interval</code> event to create a sample-accurate ticker. We'll also use a <code>random</code> beahvior to change up our samples.</p>

  <p><code>interval :: AudioContext -> Event Number -> Event Number</code> in wags is similar to <a href=""><code>interval :: Int -> Event Instant</code></a> from the <code>Event</code> library with a few important exceptions.</p>

  <ul>
    <li>The wags interval works in seconds (<code>Number</code>) instead of milliseconds.</li>
    <li>The wags interval needs an audio context to work.</li>
    <li>The wags interval gets its timing from an <code>Event Number</code> instead of a plain old <code>Number</code>. This is necessary to have variable rates.</li>
  </ul>

  <blockquote><code>interval</code> works fine for a stream of events where each event is separated by more than ~100 milliseconds. For anything faster, you'll likely want to use <code>requestAnimationLoop</code> coupled with a local state, as it will be more efficient for older and battery-sensitive devices.</blockquote>

  <p>In the following example, we use <code>interval</code> to control the playback rate of an analogue synth. We'll also use a custom behavior called <code>random</code> to control the pitch.</p>

  <p>One important optimization we make here is the use of the function <code>memoize</code>. Whenever we're dealing with audio-clock timing, we want to limit the number of subscriptions to receive events from the audio clock. Ideally, there is only one subscription that takes a reading of the clock as a single source of truth. Because we are in PureScript-land, events (like everything else), are referrentially transparent, meaning that new ones will get created every time you use them (just like a new <code>2</code> is created every time you type the value <code>2</code>: they don't all refer to one uber-<code>2</code>). To sync all the events to the <i>same</i> source, we use <code>memoize</code>. While this optimization is not necessary, I recommend it: it will make sure the timing is 100% accurate at a very small energy cost (meaning <code>memoize</code> will eat up slightly more power from a phone's battery, but still not much).</p>

  <pre><code>@txt@</code></pre>

  @ex2@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))(d.value)(O1)({txt:N(ur(ue)(M1)),ex2:N(rt(du()(o)(ar({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=I(P(c))(a.startStop.start)(Y(E(o))(void 0)),f=function(i){return Vi(o)(i)(function(m){var s=p(k)(function(){var ot=Le(ga)(.01);return function(vt){return ot(on(vt))}}())(m),_=p(k)(Ua)(m),v=I(P(c))(_t())(p(k)(function(){var ot=Eo()(ss);return function(vt){return ot(P1(vt))}}())(_)),D=p(k)(function(ot){return fs(function(vt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:vt}}(ot))})(s),l=p(k)(function(ot){return fs(function(vt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:vt}}(ot))})(s),g=p(k)(function(ot){return fs(function(vt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:vt}}(ot))})(s);return[Ma(Ss(ms)(0)(v))(function(ot){return function(vt){return Pt(it)(2)([qr(it)(0)(p(k)(Fn()(Rn))(g))([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1e3,q:20})([ot])]),qr(it)(0)(p(k)(Fn()(Rn))(l))([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([ot])]),qr(it)(0)(p(k)(Fn()(Rn))(D))([wc(RD(kt(yt()(z(z(At)(RC)()()()({reflectSymbol:function(){return"q"}}))(SD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:4e3,q:20})([ot])])])}})]})};return Be(o)([Be(o)([ur(ue)("tempo"),To(o)(q_(uu)(C(c))(Y(E(o)))(Ja(uu)(K(Qo)(ho.value)("range"))(function(){return Ja(uu)(K(uf)(Ti.value)("0"))(function(){return Ja(uu)(K(af)(hi.value)("100"))(function(){return Ja(uu)(K(ff)(Fi.value)("1"))(function(){return Ja(U_)(K(cf)($i.value)("50"))(function(){return K(of)(xi.value)(Xe(function(){var i=rr(c)(Je)(Xf(hr)(qf)(n.slider)),m=Yn(Ea)(zc);return function(s){return i(m(Yc(s)))}}()))})})})})})))([])]),Tn(o)(Qr(Xt)(C(c))(p(k)(function(){var i=K(pr)(_r.value);return function(m){return i(Xe(T(m)))}}()))([Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i();var s=oa(or)(),_=Ei(E(o))(nt.create)(w1)(zE(s)(.91)(p(k)(Ia(0)(.42)(100)(1.4))(a.slider))),v=Bc(s)(f(_))(),D=j(tt)(v)(xn(or)(s));return t(j(tt)(D)(n.startStop.start(void 0)))(),n.startStop.stop(j(tt)(D)(xn(or)(s)))()}}),Ze(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))})]))([ln(ue)(dr(Xt)(C(c))([X(k)(u)("Turn on"),X(k)(a.startStop.stop)("Turn off")]))])])}})))})}}};var R1=function(){return d.value}(),Oh=function(){return me({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(R1)({})}();var L1=function(){return d.value}(),Ph=function(){return me({reflectType:function(){return`<section>
  <h2>Events in Wags</h2>
  <p>Wags follows a consistent pattern: every audio unit accepts an event containing a <code>newtype</code> around a <code>Variant</code> of parameters that can be changed. As a motivating example, let's look at the definition of <code>sinOsc</code>.</p>

  <pre><code>sinOsc
  :: forall i outputChannels event payload
   . IsEvent event
  => Common.InitialSinOsc i
  => i
  -> event C.SinOsc
  -> C.Node outputChannels "" C.G_ event payload</code></pre>

  <p>In that defintion, <code>SinOsc</code> is a <code>newtype</code> that is defined as such:</p>

  <pre><code>newtype SinOsc = SinOsc
  (Variant (frequency :: AudioParameter, onOff :: AudioOnOff))</code></pre>

  <p>Informally, the <code>sinOsc</code> function listens to an event that contains one of several possible instructions, including changing the frequency and turning the oscillator on or off. Let's see a few more examples:</p>

    <table>
    <tr>
      <th>Audio Unit</th>
      <th>Newtype name</th>
      <th>Variant</th>
    </tr>
    <tr>
      <td><code>delay</code></td>
      <td><code>Delay</code></td>
      <td><code>Variant (delayTime :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>highshelf</code></td>
      <td><code>Highshelf</code></td>
      <td><code>Variant (frequency :: AudioParameter, gain :: AudioParameter)</code></td>
    </tr>
    <tr>
      <td><code>playBuf</code></td>
      <td><code>PlayBuf</code></td>
      <td><code>Variant (playbackRate :: AudioParameter, buffer :: BrowserAudioBuffer, duration :: Maybe Number, bufferOffset :: Number, onOff :: AudioOnOff)</code></td>
    </tr>
  </table>

  <p>In practice, you'll never need to use newtypes presented above. The <code>WAGS.Properties</code> module has highly-overloaded smart constructors for all these values. For example, to use an <code>Event Number</code> to set the frequency of a <code>sinOsc</code>, you'd write <code>frequency &lt;$&gt; event</code> instead of <code>SinOsc &lt;&lt;&lt; inj (Proxy :: _ "frequency") &lt;&lt;&lt; AudioParameter &lt;&lt;&lt; { o: 0.0, t: _linearRamp, n: _ } &lt;$&gt; event</code>. Of course, you <i>can</i> write the longer format, but life is short and keystrokes are precious! Below is a table showing the varoius smart constructors available and the units to which they apply.</p>


    <table>
    <tr>
      <th>Constructor</th>
      <th>Type</th>
      <th>Receptive Audio Units</th>
    </tr>
    <tr>
      <td><code>bufferOffset</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>delayTime</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>delay</code></li></ul></td>
    </tr>
    <tr>
      <td><code>duration</code></td>
      <td><code>Maybe Number</code></td>
      <td><ul><li><code>playBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>frequency</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowpass</code></li>
          <li><code>lowshelf</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>gain</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>gain</code></li>
          <li><code>highshelf</code></li>
          <li><code>lowshelf</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>loopEnd</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>loopStart</code></td>
      <td><code>Number</code></td>
      <td><ul><li><code>loopBuf</code></li></ul></td>
    </tr>
    <tr>
      <td><code>offset</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>onOff</code></td>
      <td><code>AudioOnOff</code></td>
      <td>
        <ul>
          <li><code>constant</code></li>
          <li><code>loopBuf</code></li>
          <li><code>periodicOsc</code></li>
          <li><code>playBuf</code></li>
          <li><code>sawtoothOsc</code></li>
          <li><code>sineOsc</code></li>
          <li><code>squareOsc</code></li>
          <li><code>triangleOsc</code></li>
        </ul>
      </td>
    </tr>
    <tr>
      <td><code>playbackRate</code></td>
      <td><code>AudioParameter</code></td>
      <td><ul><li><code>loopBuf</code></li><li>playBuf</li></ul></td>
    </tr>
    <tr>
      <td><code>q</code></td>
      <td><code>AudioParameter</code></td>
      <td>
        <ul>
          <li><code>allpass</code></li>
          <li><code>bandpass</code></li>
          <li><code>highpass</code></li>
          <li><code>lowpass</code></li>
          <li><code>notch</code></li>
          <li><code>peaking</code></li>
        </ul>
      </td>
    </tr>
  </table>

  <p>You can use this smart-constructor pattern to transform any <code>Event</code> into something that Wags can consume. For example:</p>

  <ul>
    <li>If you have an <code>Event Number</code> called <code>myFreq</code> and you'd like it to control the frequency of a band-pass filter, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq)</code>.</li>
    <li>If you have an <code>Event Number</code> called <code>myQ</code> and you'd like it to control the Q value of the same bandpass, you can write <code>bandpass 440.0 (frequency &lt;$&gt; myFreq &lt;|&gt; q &lt;$&gt; myQ)</code> <i>or</i> <code>bandpass 440.0 $ oneOf [frequency &lt;$&gt; myFreq, q &lt;$&gt; myQ]</code>.</li>
    <li>If you'd like <code>myFreq</code> <i>only</i> to have an effect when it's over <code>1000.0</code>, you can write <code>bandpass 440.0 (frequency &lt;$&gt; filter (_ > 1000.0) myFreq &lt;|&gt; q &lt;$&gt; myQ)</code>.</li>
  </ul>

  <p>None of these transformations are unique to Wags:</p>
  <ul>
    <li>Because <code>Event</code> implements <a href=""><code>Functor</code></a>, you can use <code>map</code> (aka <code>&lt;$&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Alt</code></a>, you can use <code>alt</code> (aka <code>&lt;|&gt;</code> above).</li>
    <li>Because <code>Event</code> implements <a href=""><code>Plus</code></a>, you can use <code>empty</code> for an event that emits nothing as well as <code>oneOf</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>Filterable</code></a>, you can use <code>filter</code>, <code>filterMap</code>, <code>partition</code>, <code>partitionMap</code>, and <code>compact</code>.</li>
    <li>Because <code>Event</code> implements <a href=""><code>IsEvent</code></a>, you can use <code>bang</code> to emit something right away, <code>sampleOn</code> to sample one event's most recent value based on another event, and all of the other functions rolled into <a href=""><code>IsEvent</code></a>.</li>
  </ul>

  <p>
    This gets to one of the core design principles of Wags. Idiomatic Wags projects use functional reactive programming as a way to "steer" web audio, and Wags aims to be a minimal viable framework to shepherd events to their web-audio destinations.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(L1)({})}();var G1=function(){return d.value}(),Ih=function(){return me({reflectType:function(){return`<section>

  <h2>Events, a primer</h2>

  <p>The <code>Event</code> and <code>Behavior</code> types in PureScript are defined as such:</p>

  <pre><code>newtype Event a =
    Event ((a -> Effect Unit) -> Effect (Effect Unit))

newtype ABehavior event a =
  ABehavior (forall b. event (a -> b) -> event b)
type Behavior = ABehavior Event
</code></pre>

  <p>Let's unpack what the contract of both types are saying.</p>

  <h3>Event</h3>

  <p>An event takes a pusher of type <code>a -> Effect Unit</code> to which you can push values of type <code>a</code>. What are the values? Whatever you want! It could be a mouse click, a slider's input, an animation loop thunk, whatever. The event returns a nested <code>Effect</code> - the outer one is triggered on event subscription and the inner one is triggered on event unsubscription. In the case of a click listener, for example, the outer effect will likely call <code>addEventListener</code> and the inner will likely call <code>removeEventListener</code>.</p>

  <p>
    When using Wags, you have to get your events from somewhere. At a minimum, you'll consume a browser interaction like a click or swipe that turns on the audio. In fact, without some form of human interaction, most browsers will block the Web Audio API from turning on.
  </p>
  <p>
    <code>Events</code> are often produced within a web framework like <a href="https://github.com/mikesol/purescript-deku">Deku</a>, Halogen or React. They don't have to be, though - you can create and consume your own events.
  </p>

  <h3>Behavior</h3>

  <p>
    The <code>Behavior</code> type takes an event that needs to be "unlocked" (meaning in the form of <code>a -> b</code>, so an <code>a</code> is needed to unlock a <code>b</code>) and unlocks it with an <code>a</code>. Behaviors don't need to produce their <code>a</code> immediately. In fact, they don't need to produce it at all: it's entirely possible to create <code>Behavior (const empty)</code> that "mutes" the event. This resembles the physical world: when we want to observe a behavior, like the weather outside or the axial rotation of the Earth, there is a time-cost to observing anything that ranges from instantaneous to infinite.
  </p>

  <p>
    In Wags, we usually want to observe the behavior of things like a mouse's position, an audio buffer's content or a random number generator.
  </p>
</section>`}})({reflectType:function(){return"@"}})()()(o)(Q(c))(d.value)(G1)({})}();var U1=function(){return d.value}(),Rh=function(t){return function(e){return function(r){return function(n){var a=function(f){return Oa(P(c))(E(o))(n)(j(tt)(e(f))(vn))},u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Events</h1>

  <h3>Clicks, wiggles and loops, oh my!</h3>
  <p>
    The true magic of web audio lies in its ability to harness the rich interactivity built into the browser. We can use mouse clicks, finger swipes and animation loops to create beautiful audio landscapes. But how can we tame the complexity of all these events in an expressive, declarative, functional manner? Enter <code>Event</code>, the abstraction that allows us to build rich reactive works using Wags.
  </p>

  @primer@
  @inWags@
  @flavors@
  @ex0@
  @ex1@
  @ex2@

  <h2>Next steps</h2>
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at different ways to specify <a @next@ style="cursor:pointer;">the numeric parameters being sent as events</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(R()(R()(R()(_n()(R()(Q(c))({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}})(o))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}})(o))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}})(o))(d.value)(U1)({next:a(M_.value),primer:N(Ih),inWags:N(Ph),flavors:N(Oh),ex0:N(hh(u)(e)(n)),ex1:N(Mh(u)(e)(n)),ex2:N(wh(u)(e)(n))})}}}};var H1=function(){return d.value}(),Nh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0 do
      let ooo n = bang $ onOff $ dt (add n) apOn
      [ playBuf tink0 (ooo 0.1)
      , playBuf tink1 (ooo 0.2)
      , playBuf tink2 (ooo 0.9)
      , playBuf tink3 (ooo 1.8)
      ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(H1)({ai0:N(rt(ht(r)(t)(function(n){return Co(Ln)(Ht(ki)(Ht(ki)(Ht(ki)(p(Lc)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return mt(n)([Pt(it)(1)(function(){var u=function(f){return Y(E(o))(Hf()(Pf)(ps()(Le(ga)(f))(v_)))};return[Qn(za)(a.tink0)(u(.1)),Qn(za)(a.tink1)(u(.2)),Qn(za)(a.tink2)(u(.9)),Qn(za)(a.tink3)(u(1.8))]}())])}})))})}}};var V1=function(){return d.value}(),Lh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\{ tink0, tink1, tink2, tink3 } -> run2_
  [ gain_ 1.0
      $ do
          let
            ooo n = bang $ onOff $ dt (add n) apOn
            mtk i =
              case i \`mod\` 4 of
                0 -> tink0
                1 -> tink1
                2 -> tink2
                _ -> tink3
          0 .. 100 &lt;#&gt;
            \\i' -> do
              let i = toNumber i'
              playBuf (mtk i')
                (ooo (0.3 + 0.3 * (i * (1.005 \`pow\` i))))</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(V1)({ai0:N(rt(ht(r)(t)(function(n){return Co(Ln)(Ht(ki)(Ht(ki)(Ht(ki)(p(Lc)(function(a){return function(u){return function(f){return function(i){return{tink0:a,tink1:u,tink2:f,tink3:i}}}}})(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Nn(Ln)(Ct(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(a){return mt(n)([Pt(it)(1)(function(){var u=function(i){return Y(E(o))(Hf()(Pf)(ps()(Le(ga)(i))(v_)))},f=function(i){var m=cu(wo)(i)(4);return m===0?a.tink0:m===1?a.tink1:m===2?a.tink2:a.tink3};return Ze(xe)(dn(0)(100))(function(i){var m=qe(i);return Qn(za)(f(i))(u(.3+.3*(m*Xi(1.005)(m))))})}())])}})))})}}};var j1=function(){return d.value}(),Wh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(j1)({ai0:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ma(_e(ge)(a)(_t()))(function(u){return function(f){return Pt(it)(.8)([hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:400,q:1})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:880,q:5})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:1200,q:10})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:2e3,q:20})([u]),hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:3e3,q:30})([u])])}})])}})))})}}};var Q1=function(){return d.value}(),Gh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(Q1)({ai0:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Ma(_e(ge)(a)(_t()))(function(u){return function(f){return Pt(it)(.8)(Ze(xe)(dn(0)(40))(fo(xa)(qe)(function(i){return hn(mn(kt(yt()(z(z(At)(En)()()()({reflectSymbol:function(){return"q"}}))(sn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:200+i*150,q:30})([u])})))}})])}})))})}}};var Y1=function(){return d.value}(),Bh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(Y1)({ai0:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return mt(n)([Ya(function(u){return Pt(it)(1)([Qn(za)(a)(_t()),go(en)(.1)([Pt(it)(.6)([u])])])})])}})))})}}};var tL=function(){return d.value}(),eL=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,0],o:0,d:10}))}},rL=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,0],o:0,d:8}))}},Zc=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return go(t)(n)([Pt(e)(a)([wc(r)(u)(f)])])}}}}}}},Uh=function(t){return function(e){return function(r){return me({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}})(o))(d.value)(tL)({txt:N(ur(ue)(`dgh d g h i =
  delay_ d [gain_ g [highpass_ h i]]

fade0 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 8.0 }

fade1 = bang
  $ P.gain
  $ AudioEnvelope { p: [1.0, 1.0, 0.0], o: 0.0, d: 10.0 }

scene buf = run2_
  [ fan1 (playBuf buf bangOn) \\b _ -> mix $ fix
      \\g0 -> gain_ 1.0
        [ b
        , dgh 0.15 0.7 1500.0
            [ fix
                \\g1 -> gain 1.0 fade1
                  [ dgh 0.4 0.5 2500.0
                      [ g0, g1 ]
                  ]
            ]
        , dgh 0.29 0.85 2000.0
            [ fix
                \\g1 -> gain_ 1.0
                  [ dgh 0.6 0.6 3500.0
                      [ g0
                      , ( fix
                            \\g2 -> gain 1.0 fade0
                              [ dgh 0.75 0.6 4000.0
                                  [ g1, g2 ]
                              , dgh 0.75 0.55 3000.0 [ b ]
                              ]
                        )
                      ]
                  ]
            ]
        ]
  ]`)),ai0:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(a){return mt(n)([Ma(Qn(za)(a)(_t()))(function(u){return function(f){return Ya(function(i){return Pt(it)(1)([u,Zc(en)(it)(Ka)(.15)(.7)(1500)([Ya(function(m){return qr(it)(1)(eL(E(o))())([Zc(en)(it)(Ka)(.4)(.5)(2500)([i,m])])})]),Zc(en)(it)(Ka)(.29)(.85)(2e3)([Ya(function(m){return Pt(it)(1)([Zc(en)(it)(Ka)(.6)(.6)(3500)([i,Ya(function(s){return qr(it)(1)(rL(E(o))())([Zc(en)(it)(Ka)(.75)(.6)(4e3)([m,s]),Zc(en)(it)(Ka)(.75)(.55)(3e3)([u])])})])])})])])})}})])}})))})}}};var aL=function(){return d.value}(),qh=function(t){return function(e){return function(r){return function(n){var a=function(u){return Oa(P(c))(E(o))(n)(j(tt)(e(u))(vn))};return me({reflectType:function(){return`<section>
  <p>
    In the <a @hwLink@ style="cursor:pointer;">hello world</a> section, we saw how to create and wire up two audio nodes: a <code>sinOsc</code>, or a sine-wave oscillator, is hooked up to a <code>gain</code> node. For some cases, feeding one audio node to another all the way up to a loudspeaker will be all you need. However, in most cases, you'll need to exploit three additional relationships:</p>
    <ul>
      <li><span style="font-weight:800px;">Many to one</span>, where many audio units pass through one.</li>
      <li><span style="font-weight:800px;">One to many</span>, where a single audio unit passes through many different ones.</li>
      <li><span style="font-weight:800px;">Feedback</span>, where an audio unit is an input to itself.</li>
    </ul>
    <p>This section will show how wags handles all three cases:</p>
    <ul>
      <li><span style="font-weight:800px;"><code>Array</code></span> is the data structure we'll use to send many audio units into one.</li>
      <li><span style="font-weight:800px;"><code>fan</code></span> is a function that we'll use to "fan" one audio node out to many.</li>
      <li><span style="font-weight:800px;"><code>fix</code></span> is the function we'll use to make an audio unit an input into itself.</li>
    </ul>
    <h2>The setup</h2>
    <p>
      To illustrate how <code>Array</code>, <code>fan</code> and <code>fix</code> work, we're going to use a few new audio units.
    </p>
      <ul>
        <li><code>delay</code>: A delay node</li>
        <li><code>bandpass</code>: A bandpass filter, meaning a filter that lets a single frequency band pass through.</li>
        <li><code>playBuf</code> and <code>loopBuf</code>: Playback or looping playback of a buffer. We'll use some MP3 files from freesound.org.</li>
      </ul>
  </section>`}})({reflectType:function(){return"@"}})()()(o)(_n()(Q(c))({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}})(o))(d.value)(aL)({hwLink:a(Gf.value)})}}}};var oL=function(){return d.value}(),Hh=function(t){return function(e){return function(r){return function(n){var a=function(f){return Oa(P(c))(E(o))(n)(j(tt)(e(f))(vn))},u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Array, fan, and fix</h1>

  <h3>The anatomy of a Wags graph</h3>

  @intro@

  <h2>Arrays</h2>

  <p>To send several audio units through one, we use an <code>Array</code>.</p>

  @code0@

  <p>PureScript <code>Array</code>-s are extremely flexible and efficient, so go to town! For example, you can <code>map</code> (aka <code>&lt;#&gt;</code> when flipped) over a range of integers to create audio units, like in the example below.</p>

  @code1@

  <h2>Fan</h2>

  <p><span style="font-weight:800;">Fan</span> takes a vector of audio signals and fans it out to multiple processing chains. If you have a single signal, you can use <code>fan1</code>. For example, if you have a looping buffer and you'd like to filter it through a bank of different filters, you can do this via fan. Fan takes two arguments:</p>

  <ul>
    <li>A vector of nodes to fan out (or a single node in the case of <code>fan1</code>).</li>
    <li>A function that accepts a reference to this/these node(s) and returns a new node that may or may not contain the input.</li>
  </ul>

  <p>Let's see an example below that fans one <code>playBuf</code> to five bandpass filters.</p>

  @code2@

  <p>Just for kicks, let's jack it up to forty bandpass filters.</p>

  @code3@

  <h2>Fix</h2>

  <p><span style="font-weight:800;">Fix</span> is a fixed point operator. It accepts itself as an argument and returns... itself \u{1F92F}. You can use <code>fix</code> to create feedback loops!</p>

  @code4@

  <blockquote>If you don't have some sort of delay line in your processing chain, either via the Web-Audio-provided delay line or a custom delay node, Web Audio will raise a runtime error. Wags doesn't check for this, so make sure you test your audio to guarantee that it's feedback-explosion-free!</blockquote>

  <p>Nothing stops you from nesting <code>fix</code>-s to create a mega-feedback loop!</p>

  <blockquote>In the example below, I've added a couple fades to make sure the experience isn't too unpleasant. We'll talk more about fades in the events section \u{1F3B8}</blockquote>

  @code5@

  <h2>Next steps</h2>
  <p>In this section, saw how to combine together audio nodes with arrays, fan one audio node to many processing chains via <code>fan</code>, and how to create a fixed point, aka feedback, for a node via <code>fix</code>. In the next section, we'll ramp up on all of the yummy <a @next@ style="cursor:pointer;">audio nodes you can use</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(R()(R()(R()(R()(R()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}})(o))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}})(o))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}})(o))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}})(o))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}})(o))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}})(o))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}})(o))(d.value)(oL)({intro:N(qh(t)(e)(r)(n)),next:a(F_.value),code0:N(Nh(u)(e)(n)),code1:N(Lh(u)(e)(n)),code2:N(Wh(u)(e)(n)),code3:N(Gh(u)(e)(n)),code4:N(Bh(u)(e)(n)),code5:N(Uh(u)(e)(n))})}}}};var zh=function(t){return function(e){return function(r){return new U(J(t)("code")(e)(new H(r)))}}},wb=function(t){return zh(t)($(C(t.Monad0().Applicative0())))};var Vh=function(t){return function(e){return function(r){return new U(J(t)("pre")(e)(new H(r)))}}},Ob=function(t){return Vh(t)($(C(t.Monad0().Applicative0())))};var lL=function(){return d.value}(),Jh=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e($_.value))(vn),u=Pa(t)(r);return me({reflectType:function(){return`<div>
  <h1>Hello world</h1>

  <h3>Wagging at 440Hz</h3>

  <p>Here's a "hello world" in Wags. In this and all the following sections, we'll start with a full example, and we'll pick it apart afterwards.</p>

  @code@
  @result@

  <h2>The <code>run</code> functions</h2>

  <p>The <code>run</code> family of functions run our audio and produces an unsubscribe function that we use to stop the audio. In this case, <code>run2_</code> does three extra things:
  <ul>
    <li>Wires up our session for two-channel audio. If the sources are mono, it will automatically scale them up to stereo.</li>
    <li>Automatically handles creation and destruction of audio contexts.</li>
    <li>Takes care of the subscription to the rendering engine.</li>
  </ul></p>

  <p>The <code>push</code> function comes from the <a href="https://github.com/mikesol/purescript-deku"><code>purescript-deku</code></a> framework and is used to register an unsubscribe effect. When we trigger the effect it in the <code>Just</code> branch of our pattern match, the audio turns off.</p>

  <h2>Audio units</h2>

  <p>The sound you hear when you play the example above is created with the statement <code>gain_ 0.15 [ sinOsc 440.0 bangOn ]</code>. The first function, <code>gain_</code>, creates a gain node with a volume of <code>0.15</code>. In WebAudio, gain ranges from <code>0.0</code> to <code>1.0</code> and can be converted to decibels using the following equation:</p>

  <pre><code>decibels = 20 * log10( gain );</code></pre>

  <p>In our case, a gain of <code>0.15</code> is roughly <code>-16.5 dB</code>.</p>

  <p>Our sine wave oscillator is set to a frequency of <code>440Hz</code>. That means that your loudspeaker or headphones will vibrate back and forth in sinusoidal motion 440 times per second, which most folks perceive as the <a href="https://en.wikipedia.org/wiki/A440_(pitch_standard)">note A</a>. And we turn on the oscillator with <code>bangOn</code>, as the default is off for <i>all</i> sound generators in Wags. This is a design decision to help preserve the hearing of those that work frequently with audio.</p>

  <h2>Next steps</h2>
  <p>Now that we have our setup running, let's explore the anatomy of a Wags graph. Irrespective of the nodes comprising the graph, there are three basic concepts you need to be familiar with before you start diving into audio units: <a @next@ style="cursor:pointer;">array, fan, and fix</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(o)(R()(_n()(R()(Q(c))({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}})(o))(d.value)(lL)({code:N(Ob(o)([wb(o)([ur(ue)(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:N(rt(ht(n)(u)(function(f){return h(ba)(void 0)})(function(f){return function(i){return mt(f)([Pt(it)(.15)([Wf(Zi)(440)(_t())])])}}))),next:Oa(P(c))(E(o))(n)(a)})}}}};var jh=ic;var Xh=function(){return function(t){return t}};var Qh=function(){return function(t){return t}};var Pb=function(){function t(){}return t.value=new t,t}();var Kh={attr:function(t){return function(e){return b({key:"height",value:L(e)})}}};var Ib=function(){function t(){}return t.value=new t,t}();var Yh={attr:function(t){return function(e){return b({key:"width",value:L(e)})}}};var Rb=function(t){return function(e){return function(r){return new U(J(t)("canvas")(e)(new H(r)))}}};var Nb=function(){function t(){}return t.value=new t,t}(),Lb={attr:function(t){return function(e){return b({key:"@self@",value:ct(e)})}}};function Qs(t){return function(){return t.getContext("2d")}}function H_(t){return function(e){return function(){t.fillStyle=e}}}function Ks(t){return function(){t.beginPath()}}function Ys(t){return function(){t.fill()}}function Wb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function Zs(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var xL=function(){return 2*Qi}(),tl=function(t){return{o:t.value0+.04,n:t.value1,t:Ho}};var FL=function(){return d.value}(),el=function(t){return function(e){return function(r){return function(n){return Y(t)(Eo(e)(Rn)({p:[r,n],o:0,d:16}))}}}},$L=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},ML=function(t){return function(e){return Y(t)(Fn(e)(Rn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var tm=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return E_(t)(n)(a)([qr(e)(u)(f)([VD(r)(i)(m)(s)])])}}}}}}}}}},Zh=function(t){return function(e){return function(r){return function(n){return function(a){return function(u){return function(f){return function(i){return function(m){return function(s){return E_(t)(n)(a)([qr(e)(u)(f)([zD(r)(i)(m)(s)])])}}}}}}}}}},wL=function(t){return function(e){return function(r){return function(n){return Y(t)(Vc(e)(Rn)({p:[r,n],o:0,d:16}))}}}},t0=400,Gb=qe(t0),OL=function(){return jt(Xa)(t0)+"px"}(),e0=600,Bb=qe(e0),PL=function(){return jt(Xa)(e0)+"px"}(),IL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},r0=function(t){return function(e){return function(r){return me({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(FL)({ex1:N(rt(du()(o)(ar({reflectSymbol:function(){return"canvas"}})()()(ar({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ar({reflectSymbol:function(){return"loading"}})()()(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())()()()())(d.value)(function(n){return function(a){var u=I(P(c))(Y(E(o))(void 0))(a.startStop.start),f=function(i){return function(m){return function(s){var _=p(k)(function(v){return new nt(v.acTime,v.value)})(jc(i)(a.slider));return[Cs(gs(kt(yt()(z(z(At)(ks)()()()({reflectSymbol:function(){return"fftSize"}}))(ys)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))({cb:function(v){return function(){return Mn(new B(v))(s)(),Mn(V.value)(s)}},fftSize:os.value})(h(cr)(Ma(Qn(za)(m)(I(P(c))(_t())(p(k)(function(){var v=ia()(Nr),D=Xn(gn)(Ia(0)(.96)(100)(1.04));return function(l){return v(tl(D(l)))}}())(_))))(function(v){return function(D){return Ya(function(l){return Pt(it)(1)([v,E_(ND(kt(yt()(z(z(At)(NC)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(ED)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())))({maxDelayTime:2.5,delayTime:1})(p(k)(function(){var g=Vc()(Nr),ot=Xn(gn)(Ia(0)(.5)(100)(2.45));return function(vt){return g(tl(ot(vt)))}}())(_))([qr(it)(.4)(p(k)(function(){var g=Fn()(Nr),ot=Xn(gn)(Ia(0)(.6)(100)(.9));return function(vt){return g(tl(ot(vt)))}}())(_))([v])]),tm(en)(it)(Ka)(.15)($(C(c)))(.7)($(C(c)))(1500)(el(E(o))()(1500)(3e3))([Ya(function(g){return qr(it)(1)($L(E(o))())([tm(en)(it)(Ka)(.4)($(C(c)))(.5)($(C(c)))(3e3)(el(E(o))()(3e3)(100))([l,g])])})]),tm(en)(it)(Ka)(.29)(p(k)(function(){var g=Vc()(Nr),ot=Xn(gn)(Ia(0)(.1)(100)(.4));return function(vt){return g(tl(ot(vt)))}}())(_))(.85)($(C(c)))(2e3)(el(E(o))()(2e3)(5e3))([Ya(function(g){return Pt(it)(1)([tm(en)(it)(Ka)(.6)(p(k)(function(){var ot=Vc()(Nr),vt=Xn(gn)(Ia(0)(.8)(100)(.3));return function(Wt){return ot(tl(vt(Wt)))}}())(_))(.6)($(C(c)))(3500)(el(E(o))()(3500)(100))([l,Ya(function(ot){return qr(it)(1)(ML(E(o))())([Zh(en)(it)(LD)(.75)(p(k)(function(){var vt=Vc()(Nr),Wt=Xn(gn)(Ia(0)(.9)(100)(.1));return function(re){return vt(tl(Wt(re)))}}())(_))(.6)($(C(c)))(4e3)(el(E(o))()(4e3)(200))([g,ot]),Zh(en)(it)(LD)(.75)(wL(E(o))()(.75)(.2))(.55)($(C(c)))(200)(el(E(o))()(200)(4e3))([v])])})])])})])])})}})))]}}};return Be(o)([Rb(o)(I(P(c))(Qr(Xt)(C(c))(Y(E(o)))([K(Yh)(Ib.value)(PL),K(Kh)(Pb.value)(OL),K(lk)(Vt.value)("width: 100%;"),K(Lb)(Nb.value)(function(){var i=rr(c)(Je)(function(m){return function(){var _=Qs(m)();return H_(_)("black")(),Zs(_)({width:Bb,height:Gb,x:0,y:0})(),void 0}});return function(m){return i(lb(m))}}())]))(p(k)(function(i){return K(Lb)(Nb.value)(function(){var m=rr(c)(Je)(function(s){return function(){var v=Qs(s)();return H_(v)("black")(),Zs(v)({width:Bb,height:Gb,x:0,y:0})(),H_(v)("rgba(255,255,255,0.2)")(),sl(i)(function(D){return function(){return Ks(v)(),Wb(v)({end:xL,radius:D.value1*40,start:0,x:D.value0.x*Bb,y:D.value0.y*Gb,useCounterClockwise:!1})(),Ys(v)()}})()}});return function(s){return m(lb(s))}}())})(a.canvas)))([]),To(o)(Qr(Xt)(C(c))(Y(E(o)))([K(Qo)(ho.value)("range"),K(uf)(Ti.value)("0"),K(af)(hi.value)("100"),K(ff)(Fi.value)("1"),K(cf)($i.value)("50"),K(ck)(Vt.value)("width: 100%;"),K(of)(xi.value)(Xe(function(){var i=rr(c)(Je)(Xf(hr)(qf)(n.slider)),m=Yn(Ea)(zc);return function(s){return i(m(Yc(s)))}}()))]))([]),Tn(o)(dr(Xt)(C(c))([Y(E(o))(K(cc)(Vt.value)("width:100%; padding:1.0rem;")),Qr(Xt)(C(c))(p(k)(function(){var i=K(pr)(_r.value);return function(m){return i(Xe(T(m)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),Ze(k)(a.startStop.stop)(function(i){return j(tt)(i)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(i){return i.value0})(r)))(X(k)(u)(Z(et))))(function(i){return function(){i(),n.startStop.loading(void 0)();var s=lr(V.value)(),_=So(q(Rr)(oa(Ir))(function(v){return q(Rr)(Ju(Ir)(v))(function(D){return q(Rr)(p(yi)(Qh())(PS(Ln)(jh)(Ct(v))(Xh()(IL))))(function(l){return q(Rr)(sr(Ir)(Jl(0)(5e4)))(function(g){var ot=bc(Hv(Sa(Ql(l.pluck0))(vc(Dv(dv()(l))))))({newSeed:mc(g),size:4});return sr(Ir)(function(){var Wt=ra(ai)(c)(function(lt){return function(){var ke=Uu(),Gt=Uu();return{x:ke,y:Gt}}})(dn(0)(127))(),re=mt(v)(f(v)(ot)(s))(),he=Et(ef)(function(lt){return function(){var ke=wr(s)();return ea(c)(Je)(ke)(function(Gt){return function(){var Tu=P_(Gt)(),ju=p(x)(function(){var Mi=Sl(Wt),ou=p(xe)(function(Dn){return function(Yo){return Yo/255}(Dn)});return function(Dn){return Mi(ou(Dn))}}())(Is(Ps)(Tu))();return n.canvas(ju)(),void 0}})()}})(),gr=j(tt)(j(tt)(j(tt)(re)(D))(xn(or)(v)))(he);return n.startStop.stop(gr)(),gr})})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(gi(_))()})(),void 0}})])]))([ln(ue)(dr(Xt)(C(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u),p(k)(T("Loading..."))(a.startStop.loading)]))])])}})))})}}};var NL=function(){return d.value}(),n0=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Wags</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-wags">Wags</a>.</p>

  <p>Wags is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Wags tackles this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Wags that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-wags/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la wags</a>!</p>
</div>`}})()()(R()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}})(o))(o)(NL)({next:Oa(P(c))(E(o))(n)(j(tt)(e(Gf.value))(vn)),ex:N(r0(a)(e)(n))})}}}};var WL=function(){return d.value}(),a0=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Merge and split</h1>

  <h3>Inputs and outputs abound!</h3>
  <p>
    Web audio allows you to merge and split arbitrary audio. This is essential when you're working with complex audio setups like 5.1 surround sound or novel headphones used in some gaming setups. Wags allows you to both split and merge arbitrary signals using Web Audio's native merger and splitter nodes.
  </p>

  <h2>Merging</h2>

  <p>Merging audio in wags looks like any node that takes multiple inputs, but instead of accepting something of type <code>AudioInput</code>, it accepts a <i>vector of audio inputs</i>.</p>

  <h2>Splitting</h2>

  <p>Splitting is the inverse operation of merging: it takes a single audio node and splits it into its separate channels. In doing so, it resembles <code>fan</code>, but instead of fanning the audio, it splits it into mono-channel audio.</p>

  <h2>Next steps</h2>
  <p>In this section, saw how to merge and split audio. In the next section, we'll look at how to work with <a ~next~ style="cursor:pointer;">custom audio worklets</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(WL)({next:Y(E(o))(K(pr)(_r.value)(Xe(T(j(tt)(e(hs.value))(vn)))))})}}}};var BL=function(){return d.value}(),u0=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Cancel</h2>
  <p>The <code>AudioCancel</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/cancelScheduledValues"><code>cancelScheduledValues</code></a> function and cancels whatever effects you programmed in the future. In the example below, we execute the following sequence:</p>
  <ol>
    <li>Play an audio file</li>
    <li>Send an event at 1.0 seconds to schedule an evenlope to modulate the audio rate starting at 1.5 seconds.</li>
    <li>Cancel the envelope at 3.0 seconds, but schedule the cancelation to take effect at 4.0 seconds.</li>
  </ol>
  <pre><code>~txt~</code></pre>
  ~cancel~
  </section>
`}})()()(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(BL)({txt:N(ur(ue)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000
            $ bang
            $ playbackRate
            $ AudioEnvelope
                { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                , o: 1.5
                , d: 30.0
                }
          delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
      ]
  ]`)),cancel:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Pt(it)(1)([_e(ge)(a)(dr(Xt)(C(c))([_t(),Ru(1e3)(Ft(c)(ia()(Rn)({p:Zn(ri)(X(xe)(dn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Ru(3e3)(Ft(c)(ia()(CC)({o:3.5})))]))])])}})))})}}};var qL=function(){return d.value}(),o0=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))(o)(qL)({txt:N(ur(ue)(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          OneOf.do
            bangOn
            delay 1000
              $ bang
              $ playbackRate
              $ AudioEnvelope
                  { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                  , o: 1.5
                  , d: 30.0
                  }
          )
      ]
  ]`)),envelope:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Pt(it)(1)([_e(ge)(a)(dr(Xt)(C(c))([_t(),Ru(1e3)(Ft(c)(ia()(Rn)({p:Zn(ri)(X(xe)(dn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}})))})}}};var zL=function(){return d.value}(),i0=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric</code> encompasses the following three functions from the Web Audio API:</p>

  <ul>
    <li><code>linearRampToValueAtTime</code> via the <code>_linear</code> transition.</li>
    <li><code>exponentialRampToValueAtTime</code> via the <code>_exponential</code> transition.</li>
    <li><code>setValueAtTime</code> via the <code>_step</code> transition.</li>
  </ul>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1000 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
          delay 2500 OneOf.do
            bang
              $ playbackRate
              $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
            bang
              $ playbackRate
              $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}})(o))(d.value)(zL)({numericEx:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Pt(it)(1)([_e(ge)(a)(Ko(P(c))(_t())(function(){return Ko(P(c))(Ru(1e3)(Ko(P(c))(Ft(c)(ia()(Nr)({n:1,o:1,t:yD})))(function(){return Ft(c)(ia()(Nr)({n:1.3,o:2,t:Ho}))})))(function(){return Ru(2500)(Ko(P(c))(Ft(c)(ia()(Nr)({n:1,o:2.5,t:yD})))(function(){return Ft(c)(ia()(Nr)({n:.7,o:3.5,t:gC}))}))})}))])])}})))})}}};var JL=function(){return d.value}(),f0=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value"><code>value</code></a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf OneOf.do
          bangOn
          delay 1500
            $ bang
            $ playbackRate
            $ AudioSudden { n: 1.4 }
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(Q(c))({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}})(o))(d.value)(JL)({suddenEx:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([Pt(it)(1)([_e(ge)(a)(dr(Xt)(C(c))([_t(),Ru(1500)(Ft(c)(ia()(yC)({n:1.4})))]))])])}})))})}}};var XL=function(){return d.value}(),c0=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks. <a href="https://www.w3.org/TR/webaudio/#ModularRouting">Nearly any audio parameter</a> can be automated this way.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf OneOf.do
      bangOn
      bang
        $ playbackRate
        $ c1
            ( gain_ 1.0
                [ constant 1.0 bangOn
                , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                ]
            )
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(R()(Q(c))({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}})(o))(o)(XL)({unitEx:N(rt(ht(r)(t)(function(n){return Ct(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(a){return mt(n)([_e(ge)(a)(dr(Xt)(C(c))([_t(),Ft(c)(ia()(dC(Bu)(Bu))(DC(Pt(it)(1)([Es(vs)(1)(_t()),Pt(it)(.2)([Oc(ds)(100)([C_(Cc)(50)(_t())])])]))))]))])}})))})}}};var KL=function(){return d.value}(),l0=function(t){return function(e){return function(r){return function(n){var a=j(tt)(e(w_.value))(vn),u=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Parameters</h1>

  <h3>Controlling our units</h3>
  <p>
    In the previous section, we saw how we can use browser events to control audio units. The Web Audio API provides a rich set of tools to control both the audio-rate and control-rate parameters of audio units. This section goes over how wags exposes those parameters.
  </p>

  ~sudden~
  ~numeric~
  ~envelope~
  ~cancel~
  ~unit~

  <h2>Next steps</h2>
  <p>In this section, we saw how to specify parameters for audio units, including using audio-rate audio units as parameters. In the next section, we'll look at how to make events <a ~next~ style="cursor:pointer;">stateful</a>.</p>
</div>`}})()()(R()(R()(_n()(R()(R()(R()(Q(c))({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}})(o))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}})(o))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}})(o))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}})(o))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}})(o))(o)(KL)({sudden:N(f0(u)(e)(n)),numeric:N(i0(u)(e)(n)),envelope:N(o0(u)(e)(n)),cancel:N(u0(u)(e)(n)),unit:N(c0(u)(e)(n)),next:Oa(P(c))(E(o))(n)(a)})}}}};var ZL=function(){return d.value}(),_0=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Tumult</h1>

  <h2>The unknown unknowns of web audio</h2>
  <p>
    As we saw in the last section on subgraphs, sometimes, you simply don't know how your audio will evolve. For example, if we are building a live coding environment and create a space where someone can basically do <i>anything</i>, working with anonymous audio nodes will not cut it. For example, imagine that they have two relatively unconnected parts of the audio graph and want to start creating cross-connections. With subgraphs, we can't pick apart the graph and say "take node X and connect it to node Y" as nodes don't have names.
  </p>

  <p>
    Tumult solves this problem. It is an entirely separate rendering engine from the one we've seen before that works via <i>diffing</i> two audio graphs and adjusting the current graph based on the diff. The biggest difference is that tumult <i>requires</i> you to give a name to each audio unit. Otherwise, the engine won't know what node corresponds to what name.
  </p>

  <p>
    For you veteran coders and audio-ers, you may bristle at the word <i>diff</i>. After all, diffing is expensive, and we are working in resource constrained environments where timing is of the essence. Fear not! Tumult uses PureScript <code>Set</code>-s to do blazingly fast diffs. That said, tumult is the slowest part of wags (meaning it is just blazingly fast instead of obscenely and indecently fast), so only use it where you can afford a performmance hit.
  </p>

  <h2>Hello tumult</h2>

  <h2>Stability in tumult</h2>

  <p>While tumult will run a diff every time its event is triggered, if you don't tear down a node, its nodes will merrily putter along with whatever events you send them. This means that you can trigger tumult events <i>only</i> when you need to change the shape of the graph.</p>

  <h2>Feeling lucky</h2>

  <p>So far, we have only seen the <code>DiffMe</code> instruction sent to tumult. There is another instruction you can send called <code>FeelingLucky</code>. <code>FeelingLucky</code> will attempt to perform its instruction and be a no-op if it is incoherent with respect to the graph. Careful with this, though! <code>DiffMe</code> guarantees that whatever you send to tumult will be what's rendered from your loudspeaker. Too many <code>FeelingLucky</code>-s, on the other hand, can lead to bugs where you're not quite sure anymore <i>what</i> is playing. That said, <code>FeelingLucky</code> is grotesquely fast, especially for larger graphs. So if it makes sense to use it, use it!</p>

  <h2>Next steps</h2>
  <p>In this section, we learned how to use tumult to create truly dynamic audio graphs that allow you to fine-tune the speed-versus-dynamism tradeoff in various ways.</p>
  <p>In the next section, we'll look at how to create audio graphs via an <a ~next~ style="cursor:pointer;">imperative API that more closely resembles Web Audio while providing additional type-safety benefits</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(ZL)({next:Y(E(o))(K(pr)(_r.value)(Xe(T(j(tt)(e(Ts.value))(vn)))))})}}}};var eW=function(){return d.value}(),p0=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
  <h1>Audio Worklets</h1>

  <h2>Supercharging our audio experiences</h2>

  <p>The Web Audio lets you do basically anything, but when anything isn't enough, there are custom Audio Worklets. </p>

  <p>Custom audio worklets come in a few shapes an sizes:</p>

  <ul>
    <li>You can <a href="https://developer.chrome.com/blog/audio-worklet/">write your own in JavaScript</a>. While this is ok for small PoCs, it will get really slow really quick.</li>
    <li>You can compile your favorite C/C++/Rust audio processors to <a href="https://developer.chrome.com/blog/audio-worklet-design-pattern/#using-audio-worklet-with-webassembly">web assembly</a>. The helpful folks at Google have created many examples that do exactly this, and you can pilfer them for fun and profit.</li>
    <li>My go-to solution is Faust. <a href="https://faust.grame.fr/">Faust</a> is the greatest audio project on Earth, and let's you build very performant audio processors with an expressive and concise syntax. Faust's <a href="https://webaudioconf.com/posts/2017_EA_60/">Web Audio integration</a> is a one-liner from the command line.</li>
  </ul>

  <p>In this example, we'll use Faust to create a three custom audio units and wire them together using Wags. The units are:</p>

  <ul>
    <li>A wicked sawtooth synth.</li>
    <li>A kickin' comb filter.</li>
    <li>Freeverb, the free-software-world's favorite reverb!</li>
  </ul>

  <h2>Faust</h2>

  <p>Here are our three faust examples:</p>

  <h3>Sawtooth</h3>

  <h3>Comb filter</h3>

  <h3>Freeverb</h3>

  <h3>Compilation</h3>

  We can compile each example using the following commands. Each command creates a self-contained JavaScript file, so all we need to do is link to it in wags-land.

  <pre><code>placeholder</code></pre>

  <h2>Wags</h2>

  <p>Wags provides a type-safe interface for declaring the API of Audio Worklets. While it's your job to make sure the API is in fact the actual API of the worklet, assuming this is correct, wags will enforce it for you. Let's see how in the following example. Here's the code:</p>

  <pre><code>placeholder</code></pre>

  <p>And here's the result:</p>

  <blockquote>placeholder</blockquote>

  <p>You can try it yourself by <a>following this trypurescript link.</a></p>

  <h3>The API contract</h3>

  <p>The <code>InitializeAudioWorklet</code> API contract is defined like so:</p>

  <pre><code>InitializeAudioWorkletNode
  { name :: Proxy name
  , numberOfInputs :: numberOfInputs
  , numberOfOutputs :: numberOfOutputs
  , outputChannelCount :: outputChannelCount
  , parameterData :: { | parameterData }
  , processorOptions :: { | processorOptions }
  }</code></pre>

  <p>And here is how the APIs parameters are used:</p>

  <table>
    <tr>
      <th>Parameter</th>
      <th>Description</th>
    </tr>
    <tr>
      <td><code>name</code></td>
      <td>The name of the worklet as defined in the JS output. When in doubt, check the JS produced by faust!</td>
    </tr>
    <tr>
      <td><code>numerOfInputs</code></td>
      <td>How many inputs does the node take? Note that this is <i>not</i> the number of channels, but rather the number of distinct inputs, like in a merger node.</td>
    </tr>
    <tr>
      <td><code>numerOfOutputs</code></td>
      <td>How many outputs does the node produce? Note that this is <i>not</i> the number of channels, but rather the number of distinct outputs, like in a splitter node.</td>
    </tr>
    <tr>
      <td><code>outputChannelCount</code></td>
      <td>How many channels are in each output? This is defined using a tuple of type-level numbers.</td>
    </tr>
    <tr>
      <td><code>parameterData</code></td>
      <td>Initial parameters for the audio node. This will also define what parameters can be sent to it via events (the record gets flipped to a <a href="https://github.com/natefaubion/purescript-variant"><code>Variant</code></a> when it is an event).</td>
    </tr>
  </table>

  <h2>Next steps</h2>
  <p>In this section, we created three audio worklet nodes using Faust and used them in the Web Audio API via wags. There is active work going on to bundle all this into a single toolchain so that Faust can be written directly in PureScript and automatically read as an Audio Worklet by wags. Until that happens, though, this is a great solution: just make sure to get the parameter names right across the language barrier! No amonut of type-safety can save you there \u{1F605}</p>
  <p>In the next section, we'll look at how to create <a ~next~ style="cursor:pointer;">mutable state in a wags graph</a>.</p>
</div>`}})()()(_n()(Q(c))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}})(o))(o)(eW)({next:Y(E(o))(K(pr)(_r.value)(Xe(T(j(tt)(e(Bf.value))(vn)))))})}}}};var nW=function(){return d.value}(),s0=function(t){return function(e){return function(r){return function(n){return me({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(o)(Q(c))(d.value)(nW)({})}}}};var uW=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import QualifiedDo.Alt as OneOf
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (switcher, text, text_)
import Deku.Core (Domable, toDOM)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe, delay)
import FRP.Event.Class (bang)
import FRP.Event.VBus (V, vbus)
import QualifiedDo.OneOfMap as O
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Channel(..), subgraph, bangOn)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Run (run2_)
import WAGS.WebAPI (BrowserAudioBuffer)

type StartStop = V (start :: Unit, stop :: Effect Unit)
type UIEvents = V (startStop :: StartStop, slider :: Unit)

bell =
  "https://freesound.org/data/previews/339/339810_5121236-lq.mp3"
    :: String

random :: Behavior Number
random = behavior \\e ->
  makeEvent \\k -> subscribe e \\f ->
    Random.random >>= k <<< f

main :: Effect Unit
main = do
  { push, event } <- create
  runInBody (switcher scene event)
  push Nothing
  launchAff_ $ bracketCtx
    \\ctx -> decodeAudioDataFromUri ctx bell >>= liftEffect
      <<< push
      <<< Just
  where
  scene
    :: forall lock payload
     . Maybe BrowserAudioBuffer
    -> Domable Effect lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ pure $ toDOM $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0
              [ subgraph $ map
                  ( \\i ->
                      OneOf.do
                        bang $ Sound $ playBuf
                          { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                          bangOn
                        delay 5000 $ bang $ Silence
                  )
                  sl
              ]
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( O.oneOfMap bang O.do
                    D.Xtype := "range"
                    D.Min := "0"
                    D.Max := "100"
                    D.Step := "1"
                    D.Value := "50"
                    D.OnInput := cb (const (push.slider unit))
                )
                []
            ]
        , D.button
            ( O.oneOfMap (map (attr D.OnClick <<< cb <<< const)) O.do
                startE $> (music >>= push.startStop.stop)
                event.startStop.stop <#>
                  (_ *> push.startStop.start unit)
            )
            [ text OneOf.do
                startE $> "Turn on"
                event.startStop.stop $> "Turn off"
            ]
        ]
`,oW=au(function(t){return Ut(function(e){return Et(t)(function(r){return function(){var a=Uu();return e(r(a))()}})})}),iW=function(){return d.value}(),fW="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",m0=function(t){return function(e){return function(r){return me({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>Sound (Node outputChannels lock payload)</code> to create a subgraph and <code>Silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>Silence</code> event. Note that, once you turn a subraph off with <code>Silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(o)(R()(R()(Q(c))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}})(o))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}})(o))(d.value)(iW)({txt:N(ur(ue)(uW)),ex1:N(rt(du()(o)(ar({reflectSymbol:function(){return"slider"}})()()(Nu({reflectSymbol:function(){return"startStop"}})()()()(ar({reflectSymbol:function(){return"loading"}})()()(ar({reflectSymbol:function(){return"start"}})()()(ar({reflectSymbol:function(){return"stop"}})()()(Hn)()()()())()()()())()()()())(Hn)()()()())()()()())(d.value)(function(n){return function(a){var u=I(P(c))(Y(E(o))(void 0))(a.startStop.start),f=Ei(E(o))(nt.create)(oW)(Iu(E(o))(function(m){return function(s){return s+1|0}})(a.slider)(0)),i=function(m){return[Pt(it)(1)([vC(p(k)(function(s){return dr(Xt)(C(c))([Y(E(o))(new ls(Qn(y_(kt(yt()(z(z(At)($C)()()()({reflectSymbol:function(){return"playbackRate"}}))(d_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:m,playbackRate:.7+Ua(s)*2})(_t()))),Ru(5e3)(Y(E(o))(_s.value))])})(f))])]};return Be(o)([Be(o)([ur(ue)("Slide me!"),To(o)(Qr(Xt)(C(c))(Y(E(o)))([K(Qo)(ho.value)("range"),K(uf)(Ti.value)("0"),K(af)(hi.value)("100"),K(ff)(Fi.value)("1"),K(cf)($i.value)("50"),K(of)(xi.value)(Xe(T(n.slider(void 0))))]))([])]),Tn(o)(Qr(Xt)(C(c))(p(k)(function(){var m=K(pr)(_r.value);return function(s){return m(Xe(T(s)))}}()))([X(k)(a.startStop.loading)(h(c)(void 0)),Ze(k)(a.startStop.stop)(function(m){return j(tt)(m)(j(tt)(t(h(c)(void 0)))(n.startStop.start(void 0)))}),Ze(k)(Pn(E(o))(I(P(c))(Y(E(o))(h(c)(void 0)))(p(k)(function(m){return m.value0})(r)))(X(k)(u)(Z(et))))(function(m){return function(){m(),n.startStop.loading(void 0)();var _=So(q(Rr)(oa(Ir))(function(v){return q(Rr)(Ju(Ir)(v))(function(D){return q(Rr)(Ct(v)(fW))(function(l){return sr(Ir)(function(){var ot=Os(i(l))(),vt=j(tt)(j(tt)(ot)(D))(xn(or)(v));return n.startStop.stop(vt)(),vt})})})}))();return t(function(){return n.startStop.start(void 0)(),Jo(gi(_))()})(),void 0}})]))([ln(ue)(dr(Xt)(C(c))([p(k)(T("Turn off"))(a.startStop.stop),p(k)(T("Turn on"))(u)]))])])}})))})}}};var lW=function(){return d.value}(),v0=function(t){return function(e){return function(r){return function(n){var a=Pa(t)(r);return $t({reflectType:function(){return`<div>
  <h1>Subgraphs</h1>

  <h2>Making audio even more dynamic</h2>
  <p>
    When we're creating video games or other types of interactive work, it's rare that we'll be able to anticipate the exact web audio graph we'll need for an entire session. As an example, imagine that in a video game a certain sound effects accompany various characters, and those characters come in and out based on your progress through the game. One way to solve this would be to anticipate the maximum number of characters that are present in a game and do a round-robin assignment of nodes in the audio graph as characters enter and leave your game. But sometimes that's not ergonomic, and in certain cases its downright inefficient. Another downside is that it does not allow for specialization of the web audio graph based on new data, like for example a character to play a custom sound once you've earned a certain badge.
  </p>

  <p>
    Subgraphs fix this problem. They provide a concise mechansim to dynamically insert audio graphs based on events.
  </p>

  ~suby~

  <h2>Go forth and be brilliant!</h2>
  <p>Thus ends the first version of the wags documentation. Applause is always welcome ~appl~! Alas, some features remain undocumented, like audio worklets and an imperative API. At some point I hope to document all of these, but hopefully this should be enough to get anyone interested up and running. If you need to use any of those features before I document them, ping me on the <a href="https://purescript.org/chat">PureScript Discord</a>. Otherwise, happy music making with Wags!</p>
</div>`}})()()(R()(R()(Q(c))({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}})(o))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}})(o))(o)(lW)({appl:N(rt(ws("\u{1F44F}")(n)(a)(function(u){return Ct(u)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(u){return function(f){return mt(u)([Pt(it)(1)([_e(ge)(f)(_t())])])}}))),suby:N(m0(a)(e)(n))})}}}};var bLt=function(t){return t},ALt={Coercible0:function(){}},pW=function(t){return function(e){var r=function(a){var u=function(f){if(f instanceof x_)return Be(o)(h(cr)(rt(Zr(o)(n0(a.setCancellation)(a.setPage)))));if(f instanceof Gf)return Be(o)(h(cr)(rt(Zr(o)(Jh(a.setCancellation)(a.setPage)))));if(f instanceof $_)return Be(o)(h(cr)(rt(Zr(o)(Hh(a.setCancellation)(a.setPage)))));if(f instanceof F_)return Be(o)(h(cr)(rt(Zr(o)(UE(a.setCancellation)(a.setPage)))));if(f instanceof hs)return Be(o)(h(cr)(rt(Zr(o)(p0(a.setCancellation)(a.setPage)))));if(f instanceof Bf)return Be(o)(h(cr)(rt(Zr(o)(Rh(a.setCancellation)(a.setPage)))));if(f instanceof M_)return Be(o)(h(cr)(rt(Zr(o)(l0(a.setCancellation)(a.setPage)))));if(f instanceof w_)return Be(o)(h(cr)(rt(Zr(o)(Sh(a.setCancellation)(a.setPage)))));if(f instanceof Ts)return Be(o)(h(cr)(rt(Zr(o)(s0(a.setCancellation)(a.setPage)))));if(f instanceof JS)return Be(o)(h(cr)(rt(Zr(o)(a0(a.setCancellation)(a.setPage)))));if(f instanceof O_)return Be(o)(h(cr)(rt(Zr(o)(v0(a.setCancellation)(a.setPage)))));if(f instanceof jS)return Be(o)(h(cr)(rt(Zr(o)(_0(a.setCancellation)(a.setPage)))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 144, column 5 - line 144, column 81): "+[f.constructor.name])};return u(a.page)},n=Iu(E(o))(function(a){if(a instanceof Wc)return function(u){return{prevPage:new B(u.curPage),curPage:a.value0,cancel:u.cancel,pageChange:!0}};if(a instanceof QD)return function(u){return{cancel:a.value0,pageChange:!1,curPage:u.curPage,prevPage:u.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 134, column 7 - line 136, column 75): "+[a.constructor.name])})(e)({prevPage:V.value,curPage:x_.value,cancel:h(c)(void 0),pageChange:!0});return[Be(o)(p(xe)(function(a){return gv(o)([kv(o)(I(P(c))(Qr(Xt)(C(c))(Y(E(o)))([K(pr)(_r.value)(Xe(T(t(new Wc(a.value0))))),K(_k)(Vt.value)("cursor:pointer;")]))(p(k)(function(u){return K(pr)(_r.value)(Xe(T(function(){return u.cancel(),t(new Wc(a.value0))()})))})(Ml(Du(c))(function(){var u=pu(Ga);return function(f){return u(function(i){return i.pageChange}(f))}}())(n))))([ur(ue)(a.value1.value0)]),_c(o)(Y(E(o))(K(Mp)(Vt.value)(function(){return a.value1.value1?"":"display:none;"}())))([ur(ue)(" | ")])])})([new nt(x_.value,new nt("Home",!0)),new nt(Gf.value,new nt("Hello world",!0)),new nt($_.value,new nt("Array, fan, and fix",!0)),new nt(F_.value,new nt("Audio units",!0)),new nt(Bf.value,new nt("Events",!0)),new nt(M_.value,new nt("Parameters",!0)),new nt(w_.value,new nt("State",!0)),new nt(O_.value,new nt("Subgraphs",!1))])),Be(o)(h(cr)(ik(o)(function(a){return r({page:a.curPage,setPage:function(u){return t(Wc.create(u))},setCancellation:function(u){return t(QD.create(u))}})})(Ml(Du(c))(function(a){return a.pageChange})(n))))]}},yLt=function(t){return{page:t,setPage:Ot(jr(Xr(un))),setCancellation:Ot(jr(Xr(un)))}},kLt=function(){var e=q(hr)(q(hr)(Si)(pb))(fE)();return ea(c)(Je)(p(Mr)(cE)(e))(function(r){return function(){var a=Sv(),u=lr(0)(),f=Wl(o)(o)(),i=fk(o)(r)(pW(f.push)(f.event))(ug(u));return Kt(x)(Et(i)(function(m){return m(a)}))(),f.push(new Wc(x_.value))()}})()};export{bLt as TopLevelSg,kLt as main,ALt as newtypeTopLevelSg_,yLt as p2tl,pW as scene};
