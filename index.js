var Db=function(t){return function(e){for(var r=e.length,n=new Array(r),u=0;u<r;u++)n[u]=t(e[u]);return n}};var qo={compose:function(t){return function(e){return function(r){return t(e(r))}}}},po=function(t){return t.compose};var K=function(t){return t.identity},Y={identity:function(t){return t},Semigroupoid0:function(){return qo}};var tr=!0;var Et=function(t){return function(e){return function(r){return t(r)(e)}}},E=function(t){return function(e){return t}};var Bf=function(t){return function(e){return e(t)}},Qc=function(t){return function(e){return t(e)}};var b=function(){function t(){}return t.value=new t,t}();var _=function(t){return t.map},er=function(t){return function(e){return function(r){return _(t)(r)(e)}}},Be=function(t){return _(t)(E(void 0))},X=function(t){return function(e){return function(r){return _(t)(E(r))(e)}}},S_=function(t){return function(e){return _(t)(E(e))}};var La={map:po(qo)},Oe={map:Db},Is=function(t){return function(e){return function(r){return _(t)(function(n){return n(r)})(e)}}};var db=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var Er=function(t){return t.reflectSymbol};var T_=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},Wa=function(t){return function(e){return e[t]}},Mu=function(t){return function(e){return function(r){var n={};for(var u in r)({}).hasOwnProperty.call(r,u)&&(n[u]=r[u]);return n[t]=e,n}}};var yb={append:function(t){return function(e){return void 0}}};var Ka={append:db};var Ct=function(t){return t.append},Ns=function(t){return{append:function(e){return function(r){return function(n){return Ct(t)(e(n))(r(n))}}}}};var M=function(t){return t.alt};var Ab=function(t){return function(e){for(var r=t.length,n=e.length,u=new Array(r*n),o=0,c=0;c<r;c++)for(var f=t[c],l=0;l<n;l++)u[o++]=f(e[l]);return u}};var x_={apply:Ab,Functor0:function(){return Oe}},Gt=function(t){return t.apply};var J=function(t){return function(e){return function(r){return Gt(t)(_(t.Functor0())(E(K(Y)))(e))(r)}}},Xn=function(t){return function(e){return function(r){return function(n){return Gt(t)(_(t.Functor0())(e)(r))(n)}}}};var I=function(t){return t.pure};var dn=function(t){return function(e){return function(r){if(e)return r;if(!e)return I(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},Kc=function(t){return function(e){return function(r){return Gt(t.Apply0())(I(t)(e))(r)}}};var kb=function(t){return function(e){for(var r=[],n=0,u=t.length;n<u;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var Uf=function(t){return t.discard};var Yc={bind:kb,Apply0:function(){return x_}},rt=function(t){return t.bind},Gn=function(t){return Et(rt(t))};var Hf=function(t){return function(e){return function(r){return function(n){return rt(t)(e(n))(r)}}}};var Go={discard:function(t){return rt(t)}};var Ya=function(t){return function(e){return rt(t)(e)(K(Y))}};var Qn=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),u=t,o=0;u!==e;)n[o++]=u,u+=r;return n[o]=u,n}},eT=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},rT=function(t){return function(e){for(var r=[],n=0,u=0;u<t;u++)r[n++]=e;return r}},qs=typeof Array.prototype.fill=="function"?eT:rT,nT=function(){function t(u,o){this.head=u,this.tail=o}var e={};function r(u){return function(o){return new t(u,o)}}function n(u){for(var o=[],c=0,f=u;f!==e;)o[c++]=f.head,f=f.tail;return o}return function(u){return function(o){return n(u(r)(e)(o))}}}(),Pu=function(t){return t.length};var gb=function(t){return function(e){return function(r){return function(n){for(var u=0,o=n.length;u<o;u++)if(r(n[u]))return t(u);return e}}}};var Eb=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var u=n.slice();return u.splice(r,1),t(u)}}}};var aT=function(){function t(e,r,n,u,o,c){var f,l,p,m,s,d,i;for(f=o+(c-o>>1),f-o>1&&t(e,r,u,n,o,f),c-f>1&&t(e,r,u,n,f,c),l=o,p=f,m=o;l<f&&p<c;)s=u[l],d=u[p],i=r(e(s)(d)),i>0?(n[m++]=d,++p):(n[m++]=s,++l);for(;l<f;)n[m++]=u[l++];for(;p<c;)n[m++]=u[p++]}return function(e){return function(r){return function(n){var u;return n.length<2?n:(u=n.slice(0),t(e,r,u,n.slice(0),0,n.length),u)}}}}();var Zc=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,u=new Array(n),o=0;o<n;o++)u[o]=t(e[o])(r[o]);return u}}};var Cb=function(t){return function(e){return t[e]}};var Ou=function(t){return function(e){return function(r){return rt(t.Bind1())(e)(function(n){return rt(t.Bind1())(r)(function(u){return I(t.Applicative0())(n(u))})})}}};var oT=String.fromCharCode(65535),iT=String.fromCharCode(0),fT=Number.POSITIVE_INFINITY,cT=Number.NEGATIVE_INFINITY;var Sb=function(t){return function(e){return function(r){return function(n){return function(u){return n<u?t:n===u?e:r}}}}};var Tb=Sb,xb=Sb;var Fb=function(t){return function(e){return t===e}};var $b=Fb,wb=Fb;var zf={eq:wb},gi={eq:$b};var te=function(t){return t.eq};var Vt=function(){function t(){}return t.value=new t,t}(),_e=function(){function t(){}return t.value=new t,t}(),me=function(){function t(){}return t.value=new t,t}();var Mb=function(t){return function(e){return t-e|0}},Pb=function(t){return function(e){return t-e}};var Ob=function(t){return function(e){return t+e|0}},Ib=function(t){return function(e){return t*e|0}},Rb=function(t){return function(e){return t+e}},Nb=function(t){return function(e){return t*e}};var la=function(t){return t.zero};var _a={add:Rb,zero:0,mul:Nb,one:1},Bo={add:Ob,zero:0,mul:Ib,one:1};var pa=function(t){return t.one};var Fn=function(t){return t.mul};var Ue=function(t){return t.add};var vu=function(t){return t.sub};var rf={sub:Pb,Semiring0:function(){return _a}},Lb={sub:Mb,Semiring0:function(){return Bo}};var el=function(t){return function(e){return vu(t)(la(t.Semiring0()))(e)}};var sa=function(){return{compare:xb(Vt.value)(me.value)(_e.value),Eq0:function(){return zf}}}(),Yr=function(){return{compare:Tb(Vt.value)(me.value)(_e.value),Eq0:function(){return gi}}}();var zt=function(t){return t.compare};var Wb=function(t){return function(e){return function(r){var n=zt(t)(e)(r);return!(n instanceof Vt)}}};var du=function(t){return function(e){return function(r){var n=zt(t)(e)(r);if(n instanceof Vt)return r;if(n instanceof me||n instanceof _e)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var qb=function(t){return function(e){return function(r){var n=Wb(t)(r)(la(e.Semiring0()));return n?r:el(e)(r)}}};var $n=function(t){return t.top};var af={top:2147483647,bottom:-2147483648,Ord0:function(){return Yr}};var wn=function(t){return t.bottom};var Bb=function(t){return t.toString()},Ub=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var uf={show:Ub},Ru={show:Bb};var Ht=function(t){return t.show};var z=function(){function t(){}return t.value=new t,t}(),U=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var Vr=function(t){return function(e){return function(r){if(r instanceof z)return t;if(r instanceof U)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var xr={map:function(t){return function(e){return e instanceof U?new U(t(e.value0)):z.value}}};var ma=function(t){return Vr(t)(K(Y))},Bn=function(){return function(t){if(t instanceof U)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var Ci={apply:function(t){return function(e){if(t instanceof U)return _(xr)(t.value0)(e);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return xr}},va={bind:function(t){return function(e){if(t instanceof U)return e(t.value0);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return Ci}};var mo=function(){return{pure:U.create,Apply0:function(){return Ci}}}();var Ft=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),$t=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var ff={map:function(t){return function(e){if(e instanceof Ft)return new Ft(e.value0);if(e instanceof $t)return new $t(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var Fa=function(t){return function(e){return function(r){if(r instanceof Ft)return t(r.value0);if(r instanceof $t)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},M_=function(){return Fa(E(z.value))(U.create)}();var Za=function(t){return t};var vo={map:function(t){return function(e){return t(e)}}};var Hb={apply:function(t){return function(e){return t(e)}},Functor0:function(){return vo}},TT={bind:function(t){return function(e){return e(t)}},Apply0:function(){return Hb}},Us={pure:Za,Apply0:function(){return Hb}},Nu={Applicative0:function(){return Us},Bind1:function(){return TT}};var zb=function(t){return Math.min(Math.abs(t),2147483647)},Vb=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},Jb=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},jb=function(t){return function(e){return t/e}};var Xb={Ring0:function(){return rf}},Qb={Ring0:function(){return Lb}};var tu=function(t){return t.mod};var ol={degree:function(t){return 1},div:jb,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return Xb}},Do={degree:zb,div:Vb,mod:Jb,CommutativeRing0:function(){return Qb}},Lu=function(t){return t.div};var cf={mempty:void 0,Semigroup0:function(){return yb}};var be=function(t){return t.mempty},Jf=function(t){return{mempty:function(e){return be(t)},Semigroup0:function(){return Ns(t.Semigroup0())}}};var Hs=function(t){return function(){return t}},Kb=function(t){return function(e){return function(){return e(t())()}}};var hi=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var Yb=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}},lf={Applicative0:function(){return tt},Bind1:function(){return Le}},Le={bind:Kb,Apply0:function(){return zs(0)}},tt={pure:Hs,Apply0:function(){return zs(0)}},Zb=Yb("functorEffect","Effect",function(){return{map:Kc(tt)}}),zs=Yb("applyEffect","Effect",function(){return{apply:Ou(lf),Functor0:function(){return Zb(0)}}}),C=Zb(20),j=zs(23),ty=function(t){return{append:Xn(j)(Ct(t))}},Si=function(t){return{mempty:Hs(be(t)),Semigroup0:function(){return ty(t.Semigroup0())}}};var ey=function(t){return function(){return{value:t}}};var Ce=function(t){return function(){return t.value}},ry=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},ze=function(t){return function(e){return function(){e.value=t}}};var ye=ey,PT=ry,Ti=function(t){return PT(function(e){var r=t(e);return{state:r,value:r}})},Kn=function(t){return function(e){return Be(C)(Ti(t)(e))}};var WT=function(){function t(e,r,n,u,o,c){var f,l,p,m,s,d,i;for(f=o+(c-o>>1),f-o>1&&t(e,r,u,n,o,f),c-f>1&&t(e,r,u,n,f,c),l=o,p=f,m=o;l<f&&p<c;)s=u[l],d=u[p],i=r(e(s)(d)),i>0?(n[m++]=d,++p):(n[m++]=s,++l);for(;l<f;)n[m++]=u[l++];for(;p<c;)n[m++]=u[p++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var _y=function(t){return function(e){return t&&e}},py=function(t){return function(e){return t||e}},sy=function(t){return!t};var nu=function(t){return t.not};var Xf=function(t){return t.disj},wa={ff:!1,tt:!0,implies:function(t){return function(e){return Xf(wa)(nu(wa)(t))(e)}},conj:_y,disj:py,not:sy};var vy=function(t){return function(e){return function(r){for(var n=e,u=r.length,o=u-1;o>=0;o--)n=t(r[o])(n);return n}}},Dy=function(t){return function(e){return function(r){for(var n=e,u=r.length,o=0;o<u;o++)n=t(n)(r[o]);return n}}};var T=function(t){return t.empty};var et=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),Qf=function(t){return function(e){return t(e.value0)(e.value1)}};var Zr=function(t){return t.value1};var Uo={map:function(t){return function(e){return new et(e.value0,t(e.value1))}}};var Pa=function(t){return t.value0};var ft=function(t){return t};var Da=function(){return ft};var tn=Da,bn=Da;var em=function(){return function(){return function(t){return Da()}}};var ur=function(t){return t.foldr};var Jr=function(t){return function(e){return ur(t)(M(e.Alt0()))(T(e))}},Ir=function(t){return function(e){return function(r){return ur(t)(function(){var n=M(e.Alt0());return function(u){return n(r(u))}}())(T(e))}}},lr=function(t){return function(e){return function(r){return ur(e)(function(){var n=J(t.Apply0());return function(u){return n(r(u))}}())(I(t)(void 0))}}},yu=function(t){return function(e){return Et(lr(t)(e))}},W_=function(t){return function(e){return lr(t)(e)(K(Y))}},Cr=function(t){return t.foldl};var rr={foldr:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof U)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof U)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof z)return be(t);if(r instanceof U)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var ky=function(t){return function(e){return function(r){return ur(t)(function(n){return function(u){return Ct(e.Semigroup0())(r(n))(u)}})(be(e))}}},Jt={foldr:vy,foldl:Dy,foldMap:function(t){return ky(Jt)(t)}};var yn=function(t){return t.foldMap};var Ho=function(t){return function(e){return yn(t)(e)(K(Y))}};var gy=function(){function t(u){return[u]}function e(u){return function(o){return[u,o]}}function r(u){return function(o){return function(c){return[u,o,c]}}}function n(u){return function(o){return u.concat(o)}}return function(u){return function(o){return function(c){return function(f){return function(l){function p(m,s){switch(s-m){case 0:return c([]);case 1:return o(t)(f(l[m]));case 2:return u(o(e)(f(l[m])))(f(l[m+1]));case 3:return u(u(o(r)(f(l[m])))(f(l[m+1])))(f(l[m+2]));default:var d=m+Math.floor((s-m)/4)*2;return u(o(n)(p(m,d)))(p(d,s))}}return p(0,l.length)}}}}}}();var Un=function(t){return t.traverse};var My=function(t){return function(e){return Un(t)(e)(K(Y))}},zo={traverse:function(t){return gy(Gt(t.Apply0()))(_(t.Apply0().Functor0()))(I(t))},sequence:function(t){return My(zo)(t)},Functor0:function(){return Oe},Foldable1:function(){return Jt}};var bl=function(){return Zc(et.create)}();var vm=function(){return Cb};var Wy=function(t){return[t]};var qy=function(){return gb(U.create)(z.value)}();var Dm=function(){return Eb(U.create)(z.value)}(),dm=function(t){return function(e){return function(r){return r.length===0?[]:Vr(r)(function(n){return Bn()(Dm(n)(r))})(qy(t(e))(r))}}};var ec=function(t){return function(e){return Ct(Ka)([t])(e)}};var Gy=function(t){return function(e){for(var r=e.length,n=Array(r),u=0;u<r;u++)n[u]=t(u)(e[u]);return n}};var Gu=function(t){return t.mapWithIndex};var pf={mapWithIndex:Gy,Functor0:function(){return Oe}};var yo=function(t){return t.foldrWithIndex};var Uu=function(t){return t.foldlWithIndex};var Jo=function(t){return t.foldMapWithIndex};var $i=function(t){return t.traverseWithIndex};var Hu=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var V_=function(t){return function(e){return new Hu(e,T(t))}};var $r=function(){function t(){}return t.value=new t,t}(),ue=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),J_=function(t){return t},vx=function(t){return new ue(t.value0,t.value1)};var Dx=function(t){var e=function(r){return function(n){var u=r,o=!1,c;function f(l,p){if(p instanceof ue&&p.value1 instanceof ue&&p.value1.value1 instanceof ue){u=new ue(p,l),n=p.value1.value1.value1;return}var m=function(d){return d instanceof ue&&d.value1 instanceof ue&&d.value1.value1 instanceof $r?new ue(t(d.value0),new ue(t(d.value1.value0),$r.value)):d instanceof ue&&d.value1 instanceof $r?new ue(t(d.value0),$r.value):$r.value},s=function(d){return function(i){var h=d,ct=!1,mt;function fe(jt,ge){if(jt instanceof ue&&jt.value0 instanceof ue&&jt.value0.value1 instanceof ue&&jt.value0.value1.value1 instanceof ue){h=jt.value1,i=new ue(t(jt.value0.value0),new ue(t(jt.value0.value1.value0),new ue(t(jt.value0.value1.value1.value0),ge)));return}return ct=!0,ge}for(;!ct;)mt=fe(h,i);return mt}};return o=!0,s(l)(m(p))}for(;!o;)c=f(u,n);return c}};return e($r.value)},j_={map:Dx};var Oa={foldr:function(t){return function(e){var r=function(){var u=function(o){return function(c){var f=o,l=!1,p;function m(s,d){if(d instanceof $r)return l=!0,s;if(d instanceof ue){f=new ue(d.value0,s),c=d.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[s.constructor.name,d.constructor.name])}for(;!l;)p=m(f,c);return p}};return u($r.value)}(),n=Cr(Oa)(Et(t))(e);return function(u){return n(r(u))}}},foldl:function(t){var e=function(r){return function(n){var u=r,o=!1,c;function f(l,p){if(p instanceof $r)return o=!0,l;if(p instanceof ue){u=t(l)(p.value0),n=p.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[p.constructor.name])}for(;!o;)c=f(u,n);return c}};return e},foldMap:function(t){return function(e){return Cr(Oa)(function(r){var n=Ct(t.Semigroup0())(r);return function(u){return n(e(u))}})(be(t))}}};var yl={append:function(t){return function(e){return ur(Oa)(ue.create)(e)(t)}}};var ym={append:function(t){return function(e){return new Hu(t.value0,Ct(yl)(t.value1)(vx(e)))}}};var zy={alt:Ct(yl),Functor0:function(){return j_}},Am=function(){return{empty:$r.value,Alt0:function(){return zy}}}();var Ky=function(t){return t()};var Yy=function(t){throw new Error(t)};var Zy=function(){return Yy};var Nx=Ky,uu=function(t){return Nx(function(){return Zy()(t)})};var Bt=function(){function t(){}return t.value=new t,t}(),ie=function(){function t(e,r,n,u){this.value0=e,this.value1=r,this.value2=n,this.value3=u}return t.create=function(e){return function(r){return function(n){return function(u){return new t(e,r,n,u)}}}},t}(),We=function(){function t(e,r,n,u,o,c,f){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c,this.value6=f}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return new t(e,r,n,u,o,c,f)}}}}}}},t}(),wi=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),Ko=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),Mi=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),ko=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),Pi=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),Q_=function(){function t(e,r,n,u){this.value0=e,this.value1=r,this.value2=n,this.value3=u}return t.create=function(e){return function(r){return function(n){return function(u){return new t(e,r,n,u)}}}},t}();var eA=function(t){return function(e){return new ie(Bt.value,t,e,Bt.value)}};var Ux=function(t){return function(e){var r=zt(t),n=function(u){var o=!1,c;function f(l){if(l instanceof Bt)return o=!0,z.value;if(l instanceof ie){var p=r(e)(l.value1);if(p instanceof me)return o=!0,new U(l.value2);if(p instanceof Vt){u=l.value0;return}u=l.value3;return}if(l instanceof We){var m=r(e)(l.value1);if(m instanceof me)return o=!0,new U(l.value2);var s=r(e)(l.value4);if(s instanceof me)return o=!0,new U(l.value5);if(m instanceof Vt){u=l.value0;return}if(s instanceof _e){u=l.value6;return}u=l.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[l.constructor.name])}for(;!o;)c=f(u);return c};return n}};var rA=function(t){return t instanceof Bt};var en=function(t){return function(e){return function(r){var n=t,u=e,o=!1,c;function f(l,p,m){if(p instanceof $r)return o=!0,m;if(p instanceof ue){if(p.value0 instanceof wi){n=l,u=p.value1,r=new ie(m,p.value0.value0,p.value0.value1,p.value0.value2);return}if(p.value0 instanceof Ko){n=l,u=p.value1,r=new ie(p.value0.value0,p.value0.value1,p.value0.value2,m);return}if(p.value0 instanceof Mi){n=l,u=p.value1,r=new We(m,p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof ko){n=l,u=p.value1,r=new We(p.value0.value0,p.value0.value1,p.value0.value2,m,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof Pi){n=l,u=p.value1,r=new We(p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5,m);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[p.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[p.constructor.name,m.constructor.name])}for(;!o;)c=f(n,u,r);return c}}},kl=function(t){return function(e){return function(r){var n=function(c){return function(f){var l=c,p=!1,m;function s(d,i){if(d instanceof $r)return p=!0,new ie(i.value0,i.value1,i.value2,i.value3);if(d instanceof ue){if(d.value0 instanceof wi)return p=!0,en(t)(d.value1)(new We(i.value0,i.value1,i.value2,i.value3,d.value0.value0,d.value0.value1,d.value0.value2));if(d.value0 instanceof Ko)return p=!0,en(t)(d.value1)(new We(d.value0.value0,d.value0.value1,d.value0.value2,i.value0,i.value1,i.value2,i.value3));if(d.value0 instanceof Mi){l=d.value1,f=new Q_(new ie(i.value0,i.value1,i.value2,i.value3),d.value0.value0,d.value0.value1,new ie(d.value0.value2,d.value0.value3,d.value0.value4,d.value0.value5));return}if(d.value0 instanceof ko){l=d.value1,f=new Q_(new ie(d.value0.value0,d.value0.value1,d.value0.value2,i.value0),i.value1,i.value2,new ie(i.value3,d.value0.value3,d.value0.value4,d.value0.value5));return}if(d.value0 instanceof Pi){l=d.value1,f=new Q_(new ie(d.value0.value0,d.value0.value1,d.value0.value2,d.value0.value3),d.value0.value4,d.value0.value5,new ie(i.value0,i.value1,i.value2,i.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[d.value0.constructor.name,i.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[d.constructor.name,i.constructor.name])}for(;!p;)m=s(l,f);return m}},u=zt(t),o=function(c){return function(f){var l=c,p=!1,m;function s(d,i){if(i instanceof Bt)return p=!0,n(d)(new Q_(Bt.value,e,r,Bt.value));if(i instanceof ie){var h=u(e)(i.value1);if(h instanceof me)return p=!0,en(t)(d)(new ie(i.value0,e,r,i.value3));if(h instanceof Vt){l=new ue(new wi(i.value1,i.value2,i.value3),d),f=i.value0;return}l=new ue(new Ko(i.value0,i.value1,i.value2),d),f=i.value3;return}if(i instanceof We){var ct=u(e)(i.value1);if(ct instanceof me)return p=!0,en(t)(d)(new We(i.value0,e,r,i.value3,i.value4,i.value5,i.value6));var mt=u(e)(i.value4);if(mt instanceof me)return p=!0,en(t)(d)(new We(i.value0,i.value1,i.value2,i.value3,e,r,i.value6));if(ct instanceof Vt){l=new ue(new Mi(i.value1,i.value2,i.value3,i.value4,i.value5,i.value6),d),f=i.value0;return}if(ct instanceof _e&&mt instanceof Vt){l=new ue(new ko(i.value0,i.value1,i.value2,i.value4,i.value5,i.value6),d),f=i.value3;return}l=new ue(new Pi(i.value0,i.value1,i.value2,i.value3,i.value4,i.value5),d),f=i.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[d.constructor.name,i.constructor.name])}for(;!p;)m=s(l,f);return m}};return o($r.value)}}},Hx=function(t){return function(e){var r=function(f){return function(l){var p=f,m=!1,s;function d(i,h){if(i instanceof $r)return m=!0,h;if(i instanceof ue){if(i.value0 instanceof wi&&i.value0.value2 instanceof Bt&&h instanceof Bt)return m=!0,en(t)(i.value1)(new ie(Bt.value,i.value0.value0,i.value0.value1,Bt.value));if(i.value0 instanceof Ko&&i.value0.value0 instanceof Bt&&h instanceof Bt)return m=!0,en(t)(i.value1)(new ie(Bt.value,i.value0.value1,i.value0.value2,Bt.value));if(i.value0 instanceof wi&&i.value0.value2 instanceof ie){p=i.value1,l=new We(h,i.value0.value0,i.value0.value1,i.value0.value2.value0,i.value0.value2.value1,i.value0.value2.value2,i.value0.value2.value3);return}if(i.value0 instanceof Ko&&i.value0.value0 instanceof ie){p=i.value1,l=new We(i.value0.value0.value0,i.value0.value0.value1,i.value0.value0.value2,i.value0.value0.value3,i.value0.value1,i.value0.value2,h);return}return i.value0 instanceof wi&&i.value0.value2 instanceof We?(m=!0,en(t)(i.value1)(new ie(new ie(h,i.value0.value0,i.value0.value1,i.value0.value2.value0),i.value0.value2.value1,i.value0.value2.value2,new ie(i.value0.value2.value3,i.value0.value2.value4,i.value0.value2.value5,i.value0.value2.value6)))):i.value0 instanceof Ko&&i.value0.value0 instanceof We?(m=!0,en(t)(i.value1)(new ie(new ie(i.value0.value0.value0,i.value0.value0.value1,i.value0.value0.value2,i.value0.value0.value3),i.value0.value0.value4,i.value0.value0.value5,new ie(i.value0.value0.value6,i.value0.value1,i.value0.value2,h)))):i.value0 instanceof Mi&&i.value0.value2 instanceof Bt&&i.value0.value5 instanceof Bt&&h instanceof Bt?(m=!0,en(t)(i.value1)(new We(Bt.value,i.value0.value0,i.value0.value1,Bt.value,i.value0.value3,i.value0.value4,Bt.value))):i.value0 instanceof ko&&i.value0.value0 instanceof Bt&&i.value0.value5 instanceof Bt&&h instanceof Bt?(m=!0,en(t)(i.value1)(new We(Bt.value,i.value0.value1,i.value0.value2,Bt.value,i.value0.value3,i.value0.value4,Bt.value))):i.value0 instanceof Pi&&i.value0.value0 instanceof Bt&&i.value0.value3 instanceof Bt&&h instanceof Bt?(m=!0,en(t)(i.value1)(new We(Bt.value,i.value0.value1,i.value0.value2,Bt.value,i.value0.value4,i.value0.value5,Bt.value))):i.value0 instanceof Mi&&i.value0.value2 instanceof ie?(m=!0,en(t)(i.value1)(new ie(new We(h,i.value0.value0,i.value0.value1,i.value0.value2.value0,i.value0.value2.value1,i.value0.value2.value2,i.value0.value2.value3),i.value0.value3,i.value0.value4,i.value0.value5))):i.value0 instanceof ko&&i.value0.value0 instanceof ie?(m=!0,en(t)(i.value1)(new ie(new We(i.value0.value0.value0,i.value0.value0.value1,i.value0.value0.value2,i.value0.value0.value3,i.value0.value1,i.value0.value2,h),i.value0.value3,i.value0.value4,i.value0.value5))):i.value0 instanceof ko&&i.value0.value5 instanceof ie?(m=!0,en(t)(i.value1)(new ie(i.value0.value0,i.value0.value1,i.value0.value2,new We(h,i.value0.value3,i.value0.value4,i.value0.value5.value0,i.value0.value5.value1,i.value0.value5.value2,i.value0.value5.value3)))):i.value0 instanceof Pi&&i.value0.value3 instanceof ie?(m=!0,en(t)(i.value1)(new ie(i.value0.value0,i.value0.value1,i.value0.value2,new We(i.value0.value3.value0,i.value0.value3.value1,i.value0.value3.value2,i.value0.value3.value3,i.value0.value4,i.value0.value5,h)))):i.value0 instanceof Mi&&i.value0.value2 instanceof We?(m=!0,en(t)(i.value1)(new We(new ie(h,i.value0.value0,i.value0.value1,i.value0.value2.value0),i.value0.value2.value1,i.value0.value2.value2,new ie(i.value0.value2.value3,i.value0.value2.value4,i.value0.value2.value5,i.value0.value2.value6),i.value0.value3,i.value0.value4,i.value0.value5))):i.value0 instanceof ko&&i.value0.value0 instanceof We?(m=!0,en(t)(i.value1)(new We(new ie(i.value0.value0.value0,i.value0.value0.value1,i.value0.value0.value2,i.value0.value0.value3),i.value0.value0.value4,i.value0.value0.value5,new ie(i.value0.value0.value6,i.value0.value1,i.value0.value2,h),i.value0.value3,i.value0.value4,i.value0.value5))):i.value0 instanceof ko&&i.value0.value5 instanceof We?(m=!0,en(t)(i.value1)(new We(i.value0.value0,i.value0.value1,i.value0.value2,new ie(h,i.value0.value3,i.value0.value4,i.value0.value5.value0),i.value0.value5.value1,i.value0.value5.value2,new ie(i.value0.value5.value3,i.value0.value5.value4,i.value0.value5.value5,i.value0.value5.value6)))):i.value0 instanceof Pi&&i.value0.value3 instanceof We?(m=!0,en(t)(i.value1)(new We(i.value0.value0,i.value0.value1,i.value0.value2,new ie(i.value0.value3.value0,i.value0.value3.value1,i.value0.value3.value2,i.value0.value3.value3),i.value0.value3.value4,i.value0.value3.value5,new ie(i.value0.value3.value6,i.value0.value4,i.value0.value5,h)))):(m=!0,uu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[i.constructor.name])}for(;!m;)s=d(p,l);return s}},n=function(f){return function(l){var p=f,m=!1,s;function d(i,h){if(h instanceof ie&&h.value0 instanceof Bt&&h.value3 instanceof Bt)return m=!0,r(i)(Bt.value);if(h instanceof ie){p=new ue(new Ko(h.value0,h.value1,h.value2),i),l=h.value3;return}if(h instanceof We&&h.value0 instanceof Bt&&h.value3 instanceof Bt&&h.value6 instanceof Bt)return m=!0,r(new ue(new Ko(Bt.value,h.value1,h.value2),i))(Bt.value);if(h instanceof We){p=new ue(new Pi(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),i),l=h.value6;return}return m=!0,uu("The impossible happened in partial function `removeMaxNode`.")}for(;!m;)s=d(p,l);return s}},u=function(f){var l=!1,p;function m(s){if(s instanceof ie&&s.value3 instanceof Bt)return l=!0,{key:s.value1,value:s.value2};if(s instanceof ie){f=s.value3;return}if(s instanceof We&&s.value6 instanceof Bt)return l=!0,{key:s.value4,value:s.value5};if(s instanceof We){f=s.value6;return}return l=!0,uu("The impossible happened in partial function `maxNode`.")}for(;!l;)p=m(f);return p},o=zt(t),c=function(f){return function(l){var p=f,m=!1,s;function d(i,h){if(h instanceof Bt)return m=!0,z.value;if(h instanceof ie){var ct=o(e)(h.value1);if(h.value3 instanceof Bt&&ct instanceof me)return m=!0,new U(new et(h.value2,r(i)(Bt.value)));if(ct instanceof me){var mt=u(h.value0);return m=!0,new U(new et(h.value2,n(new ue(new wi(mt.key,mt.value,h.value3),i))(h.value0)))}if(ct instanceof Vt){p=new ue(new wi(h.value1,h.value2,h.value3),i),l=h.value0;return}p=new ue(new Ko(h.value0,h.value1,h.value2),i),l=h.value3;return}if(h instanceof We){var fe=function(){return h.value0 instanceof Bt&&h.value3 instanceof Bt&&h.value6 instanceof Bt}(),ct=o(e)(h.value4),jt=o(e)(h.value1);if(fe&&jt instanceof me)return m=!0,new U(new et(h.value2,en(t)(i)(new ie(Bt.value,h.value4,h.value5,Bt.value))));if(fe&&ct instanceof me)return m=!0,new U(new et(h.value5,en(t)(i)(new ie(Bt.value,h.value1,h.value2,Bt.value))));if(jt instanceof me){var mt=u(h.value0);return m=!0,new U(new et(h.value2,n(new ue(new Mi(mt.key,mt.value,h.value3,h.value4,h.value5,h.value6),i))(h.value0)))}if(ct instanceof me){var mt=u(h.value3);return m=!0,new U(new et(h.value5,n(new ue(new ko(h.value0,h.value1,h.value2,mt.key,mt.value,h.value6),i))(h.value3)))}if(jt instanceof Vt){p=new ue(new Mi(h.value1,h.value2,h.value3,h.value4,h.value5,h.value6),i),l=h.value0;return}if(jt instanceof _e&&ct instanceof Vt){p=new ue(new ko(h.value0,h.value1,h.value2,h.value4,h.value5,h.value6),i),l=h.value3;return}p=new ue(new Pi(h.value0,h.value1,h.value2,h.value3,h.value4,h.value5),i),l=h.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[h.constructor.name])}for(;!m;)s=d(p,l);return s}};return c($r.value)}},da={foldr:function(t){return function(e){return function(r){if(r instanceof Bt)return e;if(r instanceof ie)return ur(da)(t)(t(r.value2)(ur(da)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return ur(da)(t)(t(r.value2)(ur(da)(t)(t(r.value5)(ur(da)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Bt)return e;if(r instanceof ie)return Cr(da)(t)(t(Cr(da)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return Cr(da)(t)(t(Cr(da)(t)(t(Cr(da)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Bt)return be(t);if(r instanceof ie)return Ct(t.Semigroup0())(yn(da)(t)(e)(r.value0))(Ct(t.Semigroup0())(e(r.value2))(yn(da)(t)(e)(r.value3)));if(r instanceof We)return Ct(t.Semigroup0())(yn(da)(t)(e)(r.value0))(Ct(t.Semigroup0())(e(r.value2))(Ct(t.Semigroup0())(yn(da)(t)(e)(r.value3))(Ct(t.Semigroup0())(e(r.value5))(yn(da)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},Zn={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Bt)return e;if(r instanceof ie)return yo(Zn)(t)(t(r.value1)(r.value2)(yo(Zn)(t)(e)(r.value3)))(r.value0);if(r instanceof We)return yo(Zn)(t)(t(r.value1)(r.value2)(yo(Zn)(t)(t(r.value4)(r.value5)(yo(Zn)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Bt)return e;if(r instanceof ie)return Uu(Zn)(t)(t(r.value1)(Uu(Zn)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof We)return Uu(Zn)(t)(t(r.value4)(Uu(Zn)(t)(t(r.value1)(Uu(Zn)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Bt)return be(t);if(r instanceof ie)return Ct(t.Semigroup0())(Jo(Zn)(t)(e)(r.value0))(Ct(t.Semigroup0())(e(r.value1)(r.value2))(Jo(Zn)(t)(e)(r.value3)));if(r instanceof We)return Ct(t.Semigroup0())(Jo(Zn)(t)(e)(r.value0))(Ct(t.Semigroup0())(e(r.value1)(r.value2))(Ct(t.Semigroup0())(Jo(Zn)(t)(e)(r.value3))(Ct(t.Semigroup0())(e(r.value4)(r.value5))(Jo(Zn)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return da}},nA=function(){return yo(Zn)(function(t){return function(e){return function(r){return new ue(t,r)}}})($r.value)}();var Y_=function(){return Bt.value}();var hm=function(t){return function(e){return function(r){return Vr(r)(Zr)(Hx(t)(e)(r))}}};var Z_=function(t){return function(e){return function(r){return function(n){var u=e(Ux(t)(r)(n));if(u instanceof z)return hm(t)(r)(n);if(u instanceof U)return kl(t)(r)(u.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[u.constructor.name])}}}};var zx=function(t){return function(e){return function(r){return function(n){var u=function(o){return function(c){return function(f){return Z_(t)(function(){var l=Vr(f)(e(f));return function(p){return U.create(l(p))}}())(o)(c)}}};return Uu(Zn)(u)(n)(r)}}}};var aA=function(t){return zx(t)(E)};var El=function(t){return t.partitionMap};var Oi=function(t){return t.filterMap};var Cl=function(t){return t.filter};var Qx=function(t){return t},hl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Sl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Kx=function(t){return t};var tp=Da(),v=Kx;var R=function(){return hl.create}();var ut=function(){return Sl.create}(),Ze=function(){var t=_(La)(_(C)(E(!0)));return function(e){return Qx(t(e))}}(),Q=function(t){return t.attr};function cA(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Au={};function xm(t){return t()}function lA(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function _A(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function pA(t){return function(e){return function(r){return function(n){var u=r;function o(f){return function(l){return e(l)(f)(n[f])}}for(var c in n)hasOwnProperty.call(n,c)&&(u=t(u)(o(c)));return u}}}}function Tl(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var tF=Object.keys||Tl(function(t){return function(){return t}});function $m(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var wm=function(t){return function(e){return function(){return delete e[t],e}}};var Mm=Tl(function(t){return function(e){return e}});var lF=cA;var vA=function(t){return function(e){return xm(function(){var n=lF(e)();return t(n)(),n})}};var DA=function(t){return function(e){return _A(e,t)}};var ku=function(t){return function(e){return vA($m(t)(e))}},ep={map:function(t){return function(e){return lA(e,t)}}},_F={mapWithIndex:DA,Functor0:function(){return ep}},Pm=function(){return ft};var rp=pA(Bf),dA=function(t){return function(e){return rp(function(r){return function(n){return function(u){return Ct(t.Semigroup0())(r)(e(n)(u))}}})(be(t))}},Df={foldl:function(t){return rp(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return ur(Jt)(t)(e)(Mm(r))}}},foldMap:function(t){return function(e){return dA(t)(E(e))}}},bA={foldlWithIndex:function(t){return rp(Et(t))},foldrWithIndex:function(t){return function(e){return function(r){return ur(Jt)(Qf(t))(e)(Tl(et.create)(r))}}},foldMapWithIndex:function(t){return dA(t)},Foldable0:function(){return Df}},pF={traverseWithIndex:function(t){return function(e){return function(r){return rp(function(n){return function(u){return function(o){return Gt(t.Apply0())(_(t.Apply0().Functor0())(Et(ku(u)))(n))(e(u)(o))}}})(I(t)(Au))(r)}}},FunctorWithIndex0:function(){return _F},FoldableWithIndex1:function(){return bA},Traversable2:function(){return ac}},ac={traverse:function(t){var e=$i(pF)(t);return function(r){return e(E(r))}},sequence:function(t){return Un(ac)(t)(K(Y))},Functor0:function(){return ep},Foldable1:function(){return Df}};var uc=function(t){return vA(wm(t))};var vF=function(){function t(c){this.fn=c}var e={},r=function(c,f){this.head=c,this.tail=f};function n(c){return new r(c,e)}function u(c){return function(f){return new r(c,f)}}function o(c){for(var f=[],l=c;l!==e;)f.push(l.head),l=l.tail;return f}return function(c){return function(f){return function(l){var p=function(s,d){return c(f(u)(l(s)))(d)},m=function(s,d,i){if(d===0)return s;var h=i[d-1];return new t(function(){var ct=m(p(h,s),d-1,i);return ct})};return function(s){for(var d=f(n)(l(s[s.length-1])),i=m(d,s.length-1,s);i instanceof t;)i=i.fn();return f(o)(i)}}}}}();var kA=function(t){return t};var CA=kA,Fl=function(t){return t};var hA=function(t){return CA(Wy(t))};var oc=function(t){if(Pu(t)>0)return new U(CA(t));if(tr)return z.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var SA=function(t){return function(e){return t(Fl(e))}};var TA=SA(Pu);var xA=function(){return SA(vm())};var FA=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},Ve=function(t){return t};var Nm=function(t){return function(e){return Math.pow(t,e)|0}};var np=isFinite;var $l=Math.floor;var Ri=function(t){return function(e){return Math.pow(t,e)}},wl=function(t){return function(e){return t%e}},ap=Math.round;var up=Math.sin;var Ni=3.141592653589793;var ic=function(){return FA(U.create)(z.value)}(),wA=function(t){if(!np(t))return 0;if(t>=Ve($n(af)))return $n(af);if(t<=Ve(wn(af)))return wn(af);if(tr)return ma(0)(ic(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},MA=function(t){return wA(ap(t))};var Ml=function(t){return wA($l(t))};var Lm=function(){var t=V_(Am);return function(e){return J_(t(e))}}();var RF=function(t){return function(e){return function(r){return aA(t)(e)(r)}}};var Wm=function(t){return nA(t)};var OA=function(t){return eA(t)(void 0)};var qm=function(t){return{append:RF(t)}};var IA=function(t){return rA(t)},RA=function(t){return function(e){return function(r){return kl(t)(e)(void 0)(r)}}};var NA={foldMap:function(t){return function(e){var r=yn(Oa)(t)(e);return function(n){return r(Wm(n))}}},foldl:function(t){return function(e){var r=Cr(Oa)(t)(e);return function(n){return r(Wm(n))}}},foldr:function(t){return function(e){var r=ur(Oa)(t)(e);return function(n){return r(Wm(n))}}}};var Gm=Y_;var LA=function(t){return{mempty:Gm,Semigroup0:function(){return qm(t)}}};var ip=function(t){return function(e){return function(r){return hm(t)(e)(r)}}};var FU=typeof Array.from=="function",$U=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",wU=typeof String.prototype.fromCodePoint=="function",MU=typeof String.prototype.codePointAt=="function";function Ol(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var u in t)({}).hasOwnProperty.call(t,u)&&(r[u]=t[u]);return r}var JA=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Mu(Er(t)(e))(r)(n)}}}}}};var jA=function(){return function(){return function(t){return function(e){return Ol(t,e)}}}},Il=function(t){return function(){return function(){return function(e){return function(r){return function(n){return Mu(Er(t)(e))(r)(n)}}}}}},Yo=function(t){return function(){return function(e){return function(r){return Wa(Er(t)(e))(r)}}}};var a={proof:function(t){return t},Coercible0:function(){}},bf=function(t){return t.proof};var ou=void 0;var mp=function(t){return t.toInt},XA=function(t){return function(e){return mp(t)(ou)}};var qa={toInt:function(t){return 8}},QA={Nat0:function(){return qa}},Eo={toInt:function(t){return 7}},KA={Nat0:function(){return Eo}},Co={toInt:function(t){return 6}},YA={Nat0:function(){return Co}},ya={toInt:function(t){return 5}},vp={Nat0:function(){return ya}},Pn={toInt:function(t){return 4}},Vn={Nat0:function(){return Pn}},On={toInt:function(t){return 3}},Aa={Nat0:function(){return On}},In={toInt:function(t){return 2}},ka={Nat0:function(){return In}},Rn={toInt:function(t){return 1}},ta={Nat0:function(){return Rn}},mr={toInt:function(t){return 0}};var pe=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var Vu={Nat0:function(){return Eo},Nat1:function(){return qa}};var Ju={Nat0:function(){return Co},Nat1:function(){return qa}};var ju={Nat0:function(){return ya},Nat1:function(){return qa}};var Xu={Nat0:function(){return Pn},Nat1:function(){return qa}};var ea={Nat0:function(){return Pn},Nat1:function(){return ya}};var Qu={Nat0:function(){return On},Nat1:function(){return qa}};var ra={Nat0:function(){return On},Nat1:function(){return ya}};var Ku={Nat0:function(){return In},Nat1:function(){return qa}};var na={Nat0:function(){return In},Nat1:function(){return ya}};var Yu={Nat0:function(){return Rn},Nat1:function(){return qa}};var aa={Nat0:function(){return Rn},Nat1:function(){return ya}};var Zu={Nat0:function(){return mr},Nat1:function(){return qa}};var ua={Nat0:function(){return mr},Nat1:function(){return ya}};var ZA={Nat0:function(){return mr},Nat1:function(){return qa}};var Km={Nat0:function(){return mr},Nat1:function(){return Eo}};var Ym={Nat0:function(){return mr},Nat1:function(){return Co}};var Rl={Nat0:function(){return mr},Nat1:function(){return ya}};var Ia={Nat0:function(){return mr},Nat1:function(){return Pn}};var Gr={Nat0:function(){return mr},Nat1:function(){return On}};var wr={Nat0:function(){return mr},Nat1:function(){return In}};var hr={Nat0:function(){return mr},Nat1:function(){return Rn}},oa={Nat0:function(){return mr},Nat1:function(){return mr}};var Zo=function(t){return t.state};function ho(t){return new Error(t)}function iu(t){return function(){throw t}}function Dp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var to=function(t){return t.throwError};var F$={throwError:iu,Monad0:function(){return lf}};var tv={catchError:Et(Dp),MonadThrow0:function(){return F$}};var ti=function(t){return t.catchError};var Ll=function(t){return function(e){return ti(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())($t.create)(e))(function(){var r=I(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(Ft.create(n))}}())}};var or={liftEffect:K(Y),Monad0:function(){return lf}},_r=function(t){return t.liftEffect};var rv=function(t){return{map:function(e){return function(r){return function(n){return _(t)(function(u){return new et(e(u.value0),u.value1)})(r(n))}}}}};var nv=function(t){return{Applicative0:function(){return uv(t)},Bind1:function(){return av(t)}}},av=function(t){return{bind:function(e){return function(r){return function(n){return rt(t.Bind1())(e(n))(function(u){var o=r(u.value0);return o(u.value1)})}}},Apply0:function(){return kp(t)}}},kp=function(t){return{apply:Ou(nv(t)),Functor0:function(){return rv(t.Bind1().Apply0().Functor0())}}},uv=function(t){return{pure:function(e){return function(r){return I(t.Applicative0())(new et(e,r))}},Apply0:function(){return kp(t)}}};var uk=function(t){return{state:function(e){var r=I(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return nv(t)}}};var ik=function(t){return function(e){var r=t(e);return r.value0}};var Ga=Math.random;var Ul=function(t){return function(e){return function(){var n=Ga(),u=(Ve(e)-Ve(t)+1)*n+Ve(t);return Ml(u)}}};var fk=function(t){return t};var L$=1,gp=2147483647,W$=function(){return gp-1|0}(),Ep=function(t){var e=function(r){return function(n){return function(u){var o=n-r|0,c=tu(Do)(u)(o),f=c<r;return f?c+n|0:c}}};return e(L$)(W$)(t)};var q$=0,G$=48271,ck=function(t){return function(e){return Bn()(ic(wl(Ve(G$)*Ve(e)+Ve(t))(Ve(gp))))}},lk=ck(q$);var B$=function(t){return t};var pk=function(){var t=function(e){return new et(fk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=lk(e.newSeed),r}())};return Zo(uk(Nu))(t)}();var eo=rv(vo),sk=_(eo)(function(t){return Ve(t)/Ve(gp)})(pk);var Cp=function(t){return ik(B$(t))};var gf=av(Nu);var Ef=kp(Nu),_k=function(t){return function(e){var r=Ve(e),n=Ve(t),u=function(f){return n+wl(f)(r-n+1)},o=_(eo)(Ve)(pk),c=Gt(Ef)(_(eo)(Ue(_a))(o))(_(eo)(Fn(_a)(2))(o));return _(eo)(function(f){return Ml(u(f))})(c)}},mk=function(t){return function(e){var r=t<=e;return r?_k(t)(e):_k(e)(t)}};var Hl=uv(Nu);var iv=function(t){return rt(gf)(mk(0)(TA(t)-1|0))(function(e){return I(Hl)(xA()(t)(e))})};var fv=function(t){return t.arbitrary};var vk={arbitrary:sk};var dk=zo;var To=function(t){return t};var Cf=function(t){return function(){return function(e){return function(r){return e[mp(t)(r)]}}}};var lv=pf,_v=Oe;var pv=Jt;var Sp=function(t){return function(e){var r=XA(t)(b.value),n=function(){return r===0?[]:Qn(0)(r-1|0)}();return _(Oe)(e)(n)}};var ga=[];var ve=function(t){return function(e){return function(r){return ec(e)(r)}}},bk=function(t){return ve(pe(ta)()(oa)()(hr))(t)(ga)};var Tp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),yk=function(){function t(){}return t.value=new t,t}(),xp=function(){function t(){}return t.value=new t,t}();var Fp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),$p=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),wp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Mp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var cu=function(){function t(){this.head=null,this.last=null,this.size=0}function e(m,s){this.queue=m,this.value=s,this.next=null,this.prev=null}function r(m){this.draining=!1,this.error=null,this.value=m,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function u(m){try{m()}catch(s){setTimeout(function(){throw s},0)}}function o(m,s){var d=new e(m,s);switch(m.size){case 0:m.head=d;break;case 1:d.prev=m.head,m.head.next=d,m.last=d;break;default:d.prev=m.last,m.last.next=d,m.last=d}return m.size++,d}function c(m){var s;switch(m.size){case 0:return null;case 1:s=m.head,m.head=null;break;case 2:s=m.last,m.head.next=null,m.last=null;break;default:s=m.last,m.last=s.prev,m.last.next=null}return s.prev=null,s.queue=null,m.size--,s.value}function f(m){var s;switch(m.size){case 0:return null;case 1:s=m.head,m.head=null;break;case 2:s=m.head,m.last.prev=null,m.head=m.last,m.last=null;break;default:s=m.head,m.head=s.next,m.head.prev=null}return s.next=null,s.queue=null,m.size--,s.value}function l(m){if(m.queue!==null){if(m.queue.last===m){c(m.queue);return}if(m.queue.head===m){f(m.queue);return}m.prev&&(m.prev.next=m.next),m.next&&(m.next.prev=m.prev),m.queue.size--,m.queue=null,m.value=null,m.next=null,m.prev=null}}function p(m,s){if(!s.draining){var d=s.puts,i=s.takes,h=s.reads,ct,mt,fe,jt,ge;for(s.draining=!0;;){if(ct=null,mt=null,fe=null,jt=s.value,ge=h.size,s.error!==null){for(jt=m.left(s.error);ct=f(d);)u(ct.cb(jt));for(;mt=f(h);)u(mt(jt));for(;fe=f(i);)u(fe(jt));break}if(jt===n&&(ct=f(d))&&(s.value=jt=ct.value),jt!==n){for(fe=f(i);ge--&&(mt=f(h));)u(mt(m.right(jt)));fe!==null&&(s.value=n,u(fe(m.right(jt))))}if(ct!==null&&u(ct.cb(m.right(void 0))),s.value===n&&d.size===0||s.value!==n&&i.size===0)break}s.draining=!1}}return r.EMPTY=n,r.putLast=o,r.takeLast=c,r.takeHead=f,r.deleteCell=l,r.drainVar=p,r}();function ro(){return new cu(cu.EMPTY)}function kk(t,e,r){return function(){var n=cu.putLast(e.takes,r);return cu.drainVar(t,e),function(){cu.deleteCell(n)}}}function gk(t,e,r){return function(){var n=cu.putLast(e.reads,r);return cu.drainVar(t,e),function(){cu.deleteCell(n)}}}function Ek(t,e,r){return function(){return r.value===cu.EMPTY&&r.error===null?(r.value=e,cu.drainVar(t,r),!0):!1}}function Ck(t,e){return function(){var r=e.value;return r===cu.EMPTY?t.nothing:(e.value=cu.EMPTY,cu.drainVar(t,e),t.just(r))}}var z$=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),V$=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),J$=function(){function t(){}return t.value=new t,t}();var Pp=function(){return{left:Ft.create,right:$t.create,nothing:z.value,just:U.create,killed:z$.create,filled:V$.create,empty:J$.value}}();var hk=function(t){return function(e){return gk(Pp,t,e)}};var hf=function(t){return function(e){return kk(Pp,t,e)}},xo=function(t){return function(e){return Ek(Pp,t,e)}};var Sk=function(t){return Ck(Pp,t)};var Sf=function(t){return t()};var no=function(t){return t.sampleOn};var ri=function(t){return t.keepLatest},Cu=function(t){return t.fold};var Jl=function(t){return function(e){return function(r){return function(n){return Oi(t.Filterable1())(Zr)(Cu(t)(function(u){return function(o){return _(Uo)(I(mo))(e(u)(o.value0))}})(r)(new et(n,z.value)))}}}},Op=function(t){return function(e){var r=function(n){return function(u){if(u instanceof z)return new U({now:n,last:z.value});if(u instanceof U)return new U({now:n,last:new U(u.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,u.constructor.name])}};return Oi(t.Filterable1())(K(Y))(Cu(t)(r)(e)(z.value))}},jl=function(t){return t.fix};var En=function(t){return function(e){return function(r){return M(t.Plus0().Alt0())(no(t)(e)(r))(no(t)(r)(_(t.Filterable1().Functor1())(Bf)(e)))}}},q=function(t){return t.bang};function sv(t){return function(e){return t===e}}var mv=sv;var Q$=function(t){return t},kt=function(t){return function(e){return t(e)}},K$=function(t){return function(e){return function(r){return function(){var u=ye(z.value)(),o=t(function(f){return ze(new U(f))(u)})(),c=e(function(f){return rt(Le)(Ce(u))(lr(tt)(rr)(function(l){return r(f(l))}))})();return J(j)(o)(c)}}}},Pt=Q$,Y$=function(t){return function(e){return function(){var n=ye(z.value)(),u=t(function(o){return function(){rt(Le)(Ce(n))(W_(tt)(rr))();var f=kt(o)(e)();return ze(new U(f))(n)()}})();return function(){return rt(Le)(Ce(n))(W_(tt)(rr))(),u()}}}},y={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}},Z$=function(t){return function(e){return function(r){return function(n){return function(){var o=ye(r)();return e(function(c){return rt(Le)(Ti(t(c))(o))(n)})()}}}}},Xl=function(t){return function(e){return function(r){return e(function(n){var u=t(n);if(u instanceof U)return r(u.value0);if(u instanceof z)return I(tt)(void 0);throw new Error("Failed pattern match at FRP.Event (line 108, column 13 - line 110, column 27): "+[u.constructor.name])})}}},vv=function(t){return Xl(function(e){var r=t(e);if(r)return new U(e);if(!r)return z.value;throw new Error("Failed pattern match at FRP.Event (line 66, column 13 - line 68, column 25): "+[r.constructor.name])})},Ql=function(){var e=ye([])();return{event:function(r){return function(){return Ti(function(u){return Ct(Ka)(u)([r])})(e)(),function(){return Ti(dm(mv)(r))(e)(),void 0}}},push:function(r){return rt(Le)(Ce(e))(lr(tt)(Jt)(function(n){return n(r)}))}}},tw=function(t){var e=Sf(Ql),r=t(e.event);return function(n){return function(){var o=kt(r.input)(e.push)(),c=kt(r.output)(n)();return J(j)(o)(c)}}},Gi=function(t){return function(e){return Pt(function(r){return function(){var u=Ql();return r(e(u.event))(),kt(t)(u.push)()}})}},xk={compact:Xl(K(Y)),separate:function(t){return{left:Xl(function(e){if(e instanceof Ft)return new U(e.value0);if(e instanceof $t)return z.value;throw new Error("Failed pattern match at FRP.Event (line 49, column 13 - line 51, column 33): "+[e.constructor.name])})(t),right:Xl(function(e){if(e instanceof $t)return new U(e.value0);if(e instanceof Ft)return z.value;throw new Error("Failed pattern match at FRP.Event (line 56, column 13 - line 58, column 32): "+[e.constructor.name])})(t)}}},lu={filter:vv,filterMap:Xl,partition:function(t){return function(e){return{yes:vv(t)(e),no:vv(function(){var r=nu(wa);return function(n){return r(t(n))}}())(e)}}},partitionMap:function(t){return function(e){return{left:Oi(lu)(function(){var r=Fa(U.create)(E(z.value));return function(n){return r(t(n))}}())(e),right:Oi(lu)(function(r){return M_(t(r))})(e)}}},Compactable0:function(){return xk},Functor1:function(){return y}},on=function(t){return Pt(function(e){return function(){var n=Ql();return e(t(n.push)(n.event))(),I(tt)(void 0)}})},ao=function(t){return function(e){return function(){return e(t)(),I(tt)(void 0)}}},O={alt:function(t){return function(e){return function(r){return function(){var u=t(r)(),o=e(r)();return J(j)(u)(o)}}}},Functor0:function(){return y}},g={empty:function(t){return I(tt)(I(tt)(void 0))},Alt0:function(){return O}},k={fold:Z$,keepLatest:Y$,sampleOn:K$,fix:tw,bang:ao,Plus0:function(){return g},Filterable1:function(){return lu}};var Fk=function(){function t(){}return t.value=new t,t}(),Dv=function(){function t(){}return t.value=new t,t}(),ew=function(){function t(){}return t.value=new t,t}(),lc=function(t){return function(e){var r=function(n){return n({parent:t,scope:"trivial",raiseId:be(Jf(Si(cf)))})(e)};return function(n){if(n instanceof $p)return Ir(Jt)(g)(r)(n.value0);if(n instanceof wp)return ri(k)(_(y)(lc(t)(e))(n.value0));if(n instanceof Mp)return r(n.value0);if(n instanceof Fp)return Pt(function(u){return function(){var c=ye(Au)(),f=kt(n.value0)(function(l){return function(){var m=e.ids(),s=ye(I(tt)(void 0))(),d=e.ids(),i=ye(I(tt)(void 0))(),h=ye(z.value)(),ct=ye(I(tt)(void 0))(),mt=e.ids(),fe=ye(Fk.value)(),jt=kt(l)(function(ge){return function(){var it=Ce(fe)();if(ge instanceof yk&&it instanceof Dv)return rt(Le)(Ce(h))(lr(tt)(rr)(function(Lr){return u(e.sendToTop(function(Wr){return{id:Wr}}(Lr)))}))();if(ge instanceof xp&&it instanceof Dv){ze(ew.value)(fe)();var ne=J(j)(J(j)(J(j)(J(j)(rt(Le)(Ce(h))(lr(tt)(rr)(function(Lr){return u(e.disconnectElement({id:Lr,parent:t,scope:mt}))})))(Ya(Le)(Ce(s))))(Ya(Le)(Ce(i))))(Kn(uc(m))(c)))(Kn(uc(d))(c));return J(j)(ze(ne)(ct))(ne)()}if(ge instanceof Tp&&it instanceof Fk){ze(Dv.value)(fe)();var se=ro(),It=kt(ge.value0({parent:t,scope:mt,raiseId:function(Lr){return Be(C)(xo(Lr)(se))}})(e))(u)(),dr=hf(se)(function(Lr){if(Lr instanceof $t)return function(){return ze(new U(Lr.value0))(h)(),Kn(ku(d)(It))(c)(),ze(It)(i)()};if(Lr instanceof Ft)return iu(Lr.value0);throw new Error("Failed pattern match at Deku.Internal (line 85, column 48 - line 90, column 49): "+[Lr.constructor.name])})();return dr()}return void 0}})();return ze(jt)(s)(),Kn(ku(m)(jt))(c)(),Ya(Le)(Ce(ct))()}})();return function(){return rt(Le)(Ce(c))(Ho(Df)(Si(cf)))(),f()}}});throw new Error("Failed pattern match at Deku.Internal (line 28, column 61 - line 102, column 22): "+[n.constructor.name])}}};var rw=function(t){return function(e){return{plant:function(r){return new Fp(bf(a)(Da()(r)))}}}},N=function(t){return function(e){return{plant:function(r){return new Mp(bf(e)(Da()(r)))}}}},Ee=function(t){return function(e){return{plant:function(r){return new $p(bf(a)(Da()(r)))}}}},nw=function(t){return t.makeText},aw=function(t){return function(e){return function(r){return _(y)(function(n){return t.setText(function(u){return{id:e,text:u}}(n))})(r)}}},uw=function(t){return function(e){return function(r){return _(y)(function(n){return function(u){if(u.value instanceof hl)return t.setProp({id:e,key:u.key,value:u.value.value0});if(u.value instanceof Sl)return t.setCb({id:e,key:u.key,value:u.value.value0});throw new Error("Failed pattern match at Deku.Control (line 72, column 24 - line 74, column 41): "+[u.value.constructor.name])}(tp(n))})(r)}}},ow=function(t){return t.makeElement},rn=function(t){var e=function(r){return function(n){return Pt(function(u){return function(){var c=n.ids();return r.raiseId(c)(),_(C)(J(j)(u(n.deleteFromCache({id:c}))))(kt(Jr(Jt)(g)([ao(nw(n)({id:c,parent:r.parent,scope:r.scope})),aw(n)(c)(t)]))(u))()}})}};return e},br=function(t){return rn(ao(t))},x=function(t){return t.plant},wk=function(t){return function(e){var r=function(n){var u=function(o){return function(c){return new et(c+1|0,new et(o,c))}};return Jl(k)(u)(n)(0)};return x(rw(a)(a))(ri(k)(Gi(r(e))(function(n){return _(y)(function(u){return M(O)(ao(new Tp(t(u.value0))))(_(y)(E(xp.value))(Cl(lu)(function(){var o=te(gi)(u.value1+1|0);return function(c){return o(Zr(c))}}())(n)))})(n)})))}},V=function(t){return function(e){return function(r){var n=function(u){return function(o){return Pt(function(c){return function(){var l=o.ids();return u.raiseId(l)(),_(C)(J(j)(c(o.deleteFromCache({id:l}))))(kt(M(O)(Jr(Jt)(g)([ao(ow(o)({id:l,parent:u.parent,scope:u.scope,tag:t})),uw(o)(l)(e)]))(lc(l)(o)(r)))(c))()}})}};return n}}},dv=function(t){return function(e){return ft}};var Mk=function(t){return function(e){return{plant:function(r){return dv(t)(e)(r)}}}},at=function(t){return function(e){return{plant:function(r){return new wp(_(y)(dv(t)(e))(r))}}}},iw=function(t){return function(e){return function(r){return Pt(function(n){return function(){var o=r.ids();return kt(M(O)(ao(r.makeRoot({id:o,root:t})))(lc(o)(r)(dv(a)(a)(e))))(n)()}})}}};var Pk=function(t){return function(e){return iw(t)(x(Ee(a)(a))(e))}},ee=function(){var t=function(e){return function(r){return Pt(function(n){return function(){var o=r.ids();return e.raiseId(o)(),_(C)(J(j)(n(r.deleteFromCache({id:o}))))(kt(ao(r.makeNoop({id:o,parent:e.parent,scope:e.scope})))(n))()}})}};return t}();var cr=function(){function t(){}return t.value=new t,t}();var pr={attr:function(t){return function(e){return v({key:"click",value:ut(e)})}}};var qt=function(){function t(){}return t.value=new t,t}();var Ip={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var Ok={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var lt={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var Ik={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}},_c={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var bv={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var Rk={attr:function(t){return function(e){return v({key:"style",value:R(e)})}}};var yv=function(t){return function(e){return function(r){return V("a")(e)(x(t)(r))}}};var Ae=function(t){return function(e){return function(r){return V("div")(e)(x(t)(r))}}},Je=function(t){return Ae(t)(T(g))};var sc=function(t){return function(e){return function(r){return V("span")(e)(x(t)(r))}}},Av=function(t){return sc(t)(T(g))};var cw=function(t){for(var e="",r="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",n=r.length,u=0;u<t;u++)e+=r.charAt(Math.floor(Math.random()*n));return e},kv=function(t){return function(e){return function(r){return function(){e!=="@portal@"&&r.units[e].main.appendChild(r.units[t].main)}}}};var Lk=function(t){return function(e){return function(){var r=t.id;e.scopes[t.scope]||(e.scopes[t.scope]=[]),e.scopes[t.scope].push(r),e.units[r]={listeners:{},parent:t.parent,scope:t.scope,main:document.createElement(t.tag)},t.parent===e.terminus&&e.terminalPtrs.push(t.id),kv(r)(t.parent)(e)()}}};function Wk(t){return function(e){return function(){var r=t.id;e.scopes[t.scope]||(e.scopes[t.scope]=[]),e.scopes[t.scope].push(r),e.units[r]={main:document.createTextNode(""),parent:t.parent,scope:t.scope},kv(r)(t.parent)(e)()}}}function gv(){return{units:{},scopes:{},unqidfr:cw(10)}}function qk(t){return function(e){return function(){var r=t.id,n=t.value;e.units[r].main.tagName==="INPUT"&&t.key==="value"?e.units[r].main.value=n:e.units[r].main.tagName==="INPUT"&&t.key==="checked"?e.units[r].main.checked=n==="true":e.units[r].main.setAttribute(t.key,n)}}}function Gk(t){return function(e){return function(){var r=t.id,n=t.value;if(t.key==="@canvas-hack@"){let o=e.units[r].main.getContext("2d");n(o)()}else{e.units[r].listeners[t.key]&&e.units[r].main.removeEventListener(t.key,e.units[r].listeners[t.key]);var u=o=>n(o)();e.units[r].main.addEventListener(t.key,u),e.units[r].listeners[t.key]=u}}}}function Bk(t){return function(e){return function(){var r=t.id;e.units[r].main.nodeValue=t.text}}}var Uk=function(t){return function(e){return function(){var r=t.id,n=t.html,u=t.verb,o=t.cache,c=t.parent,f=t.scope;t.parent===e.terminus&&e.terminalPtrs.push(t.id);for(var l=Object.entries(o),p=0;p<l.length;p++){var m=l[p][0];l[p][1]===!0?n=n.replace(u+m+u,'data-deku-attr-internal="'+m+'"'):n=n.replace(u+m+u,'<span style="display:contents;" data-deku-elt-internal="'+m+'"></span>')}var s=document.createElement("div");s.innerHTML=n.trim(),e.scopes[t.dkScope]||(e.scopes[t.dkScope]=[]),e.scopes[t.dkScope].push(r),e.units[r]={listeners:{},scope:t.dkScope,parent:c,main:s.firstChild},s.querySelectorAll("[data-deku-attr-internal]").forEach(function(d){var i=d.getAttribute("data-deku-attr-internal");e.units[i+f]={listeners:{},main:d,scope:t.dkScope},e.scopes[t.dkScope].push(i+f)}),s.querySelectorAll("[data-deku-elt-internal]").forEach(function(d){var i=d.getAttribute("data-deku-elt-internal");e.units[i+f]={listeners:{},main:d,scope:t.dkScope},e.scopes[t.dkScope].push(i+f)}),kv(r)(c)(e)()}}};function Hk(t){return function(e){return function(){var r=t.id;e.units[r]={main:t.root}}}}function zk(t){return function(e){return function(){var r=t.id;e.units[r]={noop:!0}}}}function Vk(t){return function(e){return function(){var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)}}}function Jk(t){return function(e){return function(){var r=t.id;e.units[r].noop||e.units[r].containingScope&&e.units[r].containingScope!==t.scope||e.units[r].main.remove()}}}function jk(t){return function(e){return function(){delete e.units[t.id]}}}function Xk(t){return function(e){return function(){var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)}}}var Qk={ids:_(C)(Ht(uf))(Ga),makeElement:Lk,makeRoot:Hk,makeText:Wk,makePursx:Uk,setProp:qk,setCb:Gk,setText:Bk,sendToTop:Xk,makeNoop:zk,deleteFromCache:jk,giveNewParent:Vk,disconnectElement:Jk};var Ra={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},ni=function(t){return t.dimap},uo=function(t){return function(e){return ni(t)(e)(K(Y))}};var Bi=function(t){return t.reflectType};var Z={pursxToElement:function(t){return function(e){return function(r){return{cache:Au,element:function(n){return function(u){return T(g)}}}}}}},Ev=function(t){return t.pursxToElement},Cn=function(){return function(t){return function(e){return function(r){return{pursxToElement:function(n){return function(u){return function(o){var c=Ev(t)(n)(b.value)(o);return{cache:ku(Bi(e)(b.value))(!0)(c.cache),element:function(f){return function(l){return M(O)(_(y)(uo(Ra)(tp)(function(p){if(p.value instanceof hl)return l.setProp({id:Bi(e)(b.value)+n,key:p.key,value:p.value.value0});if(p.value instanceof Sl)return l.setCb({id:Bi(e)(b.value)+n,key:p.key,value:p.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 3852, column 38 - line 3862, column 24): "+[p.value.constructor.name])}))(Yo(r)()(b.value)(o)))(c.element(f)(l))}}}}}}}}}}},L=function(){return function(t){return function(e){return function(r){return{pursxToElement:function(n){return function(u){return function(o){var c=Yo(r)()(b.value)(o),f=Ev(t)(n)(b.value)(o);return{cache:ku(Bi(e)(b.value))(!1)(f.cache),element:function(l){return function(p){return M(O)(lc(Bi(e)(b.value)+n)(p)(c))(f.element(l)(p))}}}}}}}}}}};var W=function(t){return function(e){return x(t)(e)}},De=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(u){return function(o){var c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids(),d=l.ids();f.raiseId(s)();var i=Ev(r)(d)(b.value)(o);return _(C)(J(j)(p(l.deleteFromCache({id:s}))))(kt(M(O)(ao(l.makePursx({id:s,parent:f.parent,cache:i.cache,scope:d,dkScope:f.scope,html:Bi(t)(u),verb:Bi(e)(n)})))(i.element(f)(l)))(p))()}})}};return c}}}}}}}},Nt=function(t){return function(){return function(){return function(e){return De(t)({reflectType:function(){return"~"}})()()(e)(b.value)}}}};function Kk(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function Yk(t){return function(e){return function(r){return r[t]=e,r}}}var Cv=qo;var hv=function(){return function(){return function(t){return function(e){return function(r){return function(n){return Yk(Er(t)(e))(r)(n)}}}}}};var Sv=Y,Zk=function(t){return function(e){return t(Kk(e))}},tg=Et(Zk)({});var dt=function(){return function(){return{defaults:Et(jA()())}}},vw=function(t){return t.defaults},bt={convertRecordOptions:function(t){return function(e){return function(r){return K(Sv)}}}},rg=function(t){return t.convertRecordOptions},ia=function(t){return t.convertOptionsWithDefaults},yt=function(){return function(t){return{convertOptions:function(e){return function(r){return tg(rg(t)(e)(b.value)(r))}}}}},Dw=function(t){return t.convertOptions},At=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var u=vw(e)(n),o=Dw(t)(r);return function(c){return u(o(c))}}}}}},dw=function(t){return t.convertOption},H=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(u){return function(o){return po(Cv)(hv()()(r)(b.value)(dw(e)(n)(b.value)(Yo(r)()(b.value)(o))))(rg(t)(n)(b.value)(o))}}}}}}}}}};var yw=function(){return function(){return function(){return function(t){return function(e){return function(r){return T_(r.type)(t)?Wa(r.type)(t)(r.value):e(r)}}}}}};var vr=function(){return function(t){return function(e){return function(r){return{type:Er(t)(e),value:r}}}}};var Aw=function(t){return uu("Data.Variant: pattern match failure ["+(t.type+"]"))},Mr=function(){return function(){return function(){return function(t){return yw()()()(t)(Aw)}}}};var Ew=function(t){return t};var Np=function(){return vr()({reflectSymbol:function(){return"nothing"}})(b.value)(void 0)}();var Dr=function(){var t=vr()({reflectSymbol:function(){return"just"}})(b.value);return function(e){return Ew(t(e))}}();var hw=JSON.parse;var Sw=JSON.stringify;var Lp=function(t){return t};var Wp=function(t){return t};var qp=function(t){return function(e){return t(e)}},Kl=function(t){return{map:function(e){return qp(_(t)(_(ff)(e)))}}};var $v=function(t){return{Applicative0:function(){return Yl(t)},Bind1:function(){return wv(t)}}},wv=function(t){return{bind:function(e){return function(r){return rt(t.Bind1())(e)(Fa(function(){var n=I(t.Applicative0());return function(u){return n(Ft.create(u))}}())(function(n){var u=r(n);return u}))}},Apply0:function(){return Dg(t)}}},Dg=function(t){return{apply:Ou($v(t)),Functor0:function(){return Kl(t.Bind1().Apply0().Functor0())}}},Yl=function(t){return{pure:function(){var e=I(t.Applicative0());return function(r){return Lp(e($t.create(r)))}}(),Apply0:function(){return Dg(t)}}};var dg=function(t){return{throwError:function(){var e=I(t.Applicative0());return function(r){return Lp(e(Ft.create(r)))}}(),Monad0:function(){return $v(t)}}};var Mv=function(t){return function(e){return{alt:function(r){return function(n){return rt(e.Bind1())(r)(function(u){if(u instanceof $t)return I(e.Applicative0())(new $t(u.value0));if(u instanceof Ft)return rt(e.Bind1())(n)(function(o){if(o instanceof $t)return I(e.Applicative0())(new $t(o.value0));if(o instanceof Ft)return I(e.Applicative0())(new Ft(Ct(t)(u.value0)(o.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[o.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[u.constructor.name])})}},Functor0:function(){return Kl(e.Bind1().Apply0().Functor0())}}}};var Pv=function(){var t=bn();return function(e){return t(Wp(e))}}();function yg(t,e,r){return t==null?e:r(t)}var jr=function(t){return yg(t,z.value,U.create)};function Zl(t){return Object.prototype.toString.call(t).slice(8,-1)}var Sg=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var Lv=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Tg=ft;var Wv=function(t){var e=to(dg(t));return function(r){return e(Lm(r))}};var qv=function(t){return function(e){return function(r){if(Zl(r)===e)return I(Yl(t))(Tg(r));if(tr)return Wv(t)(new Lv(e,Zl(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var Gv=function(t){return qv(t)("String")};var $g=function(){function t(){}return t.value=new t,t}(),wg=function(){function t(){}return t.value=new t,t}(),cM=function(){function t(){}return t.value=new t,t}();var Gp=function(){function t(){}return t.value=new t,t}(),Bp=function(){function t(){}return t.value=new t,t}(),Mg=function(){function t(){}return t.value=new t,t}(),Pg=function(){function t(){}return t.value=new t,t}(),Uv=function(){function t(){}return t.value=new t,t}(),Og=function(){function t(){}return t.value=new t,t}(),Ig=function(){function t(){}return t.value=new t,t}();var Rg=function(t){return t},Ng=function(t){return t},Lg=function(t){return t},Wg=function(t){return t},qg=function(t){return t},Gg=function(t){return t};var Bg=function(t){return t};var Ug=function(t){return t};var Hg=function(t){return t},zg=function(t){return t};var Hv=function(){function t(){}return t.value=new t,t}(),Vg=function(){function t(){}return t.value=new t,t}(),Jg=function(){function t(){}return t.value=new t,t}(),zv=function(){function t(){}return t.value=new t,t}(),jg=function(){function t(){}return t.value=new t,t}();var Vv=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Jv=function(){function t(){}return t.value=new t,t}();var nr=function(t){return function(e){return function(r){return{mix:function(n){return vr()({reflectSymbol:function(){return"node"}})(b.value)(n)}}}}},Xg=function(t){return function(e){return function(r){return{mix:function(n){return vr()({reflectSymbol:function(){return"dynamicChannels"}})(b.value)(n)}}}}},Qg=function(t){return function(e){return function(r){return{mix:function(n){return vr()({reflectSymbol:function(){return"eventfulNode"}})(b.value)(n)}}}}};var G=function(t){return function(e){return function(r){return{mix:function(n){return vr()({reflectSymbol:function(){return"fixedChannels"}})(b.value)(n)}}}}};var je=function(t){return t.mix};var Kg=function(){return vr()({reflectSymbol:function(){return"2x"}})(b.value)(void 0)}();var ln=function(t){return function(e){return function(r){var n=function(u){return u({parent:t,scope:"trivial",raiseId:be(Jf(Si(cf)))})(e)};return Mr()()()({fixedChannels:function(u){return Ir(Jt)(g)(n)(u)},eventfulNode:function(u){return ri(k)(_(y)(ln(t)(e))(u))},node:n,dynamicChannels:function(u){return Pt(function(o){return function(){var f=ye(Au)(),l=kt(u)(function(p){return function(){var s=e.ids(),d=ye(I(tt)(void 0))(),i=e.ids(),h=ye(I(tt)(void 0))(),ct=ye(z.value)(),mt=ye(I(tt)(void 0))(),fe=e.ids(),jt=ye($g.value)(),ge=kt(p)(function(Nr){return function(){var ne=Ce(jt)();if(Nr instanceof Jv&&ne instanceof wg){ze(cM.value)(jt)();var se=J(j)(J(j)(J(j)(J(j)(rt(Le)(Ce(ct))(lr(tt)(rr)(function(Wr){return o(e.disconnectXFromY({from:Wr,to:t}))})))(Ya(Le)(Ce(d))))(Ya(Le)(Ce(h))))(Kn(uc(s))(f)))(Kn(uc(i))(f));return J(j)(ze(se)(mt))(se)()}if(Nr instanceof Vv&&ne instanceof $g){ze(wg.value)(jt)();var It=ro(),dr=kt(Nr.value0({parent:t,scope:fe,raiseId:function(Wr){return Be(C)(xo(Wr)(It))}})(e))(o)(),Lr=hf(It)(function(Wr){if(Wr instanceof $t)return function(){return ze(new U(Wr.value0))(ct)(),Kn(ku(i)(dr))(f)(),ze(dr)(h)()};if(Wr instanceof Ft)return iu(Wr.value0);throw new Error("Failed pattern match at WAGS.Core (line 1046, column 46 - line 1051, column 47): "+[Wr.constructor.name])})();return Lr()}return void 0}})();return ze(ge)(d)(),Kn(ku(s)(ge))(f)(),Ya(Le)(Ce(mt))()}})();return function(){return rt(Le)(Ce(f))(Ho(Df)(Si(cf)))(),l()}}})}})(r)}}};var lM=function(){function t(){}return t.value=new t,t}(),_M=function(){function t(){}return t.value=new t,t}(),pM=function(){function t(){}return t.value=new t,t}(),sM=function(){function t(){}return t.value=new t,t}(),mM=function(){function t(){}return t.value=new t,t}(),vM=function(){function t(){}return t.value=new t,t}(),DM=function(){function t(){}return t.value=new t,t}(),dM=function(){function t(){}return t.value=new t,t}(),bM=function(){function t(){}return t.value=new t,t}(),yM=function(){function t(){}return t.value=new t,t}(),AM=function(){function t(){}return t.value=new t,t}(),kM=function(){function t(){}return t.value=new t,t}(),gM=function(){function t(){}return t.value=new t,t}(),EM=function(){function t(){}return t.value=new t,t}(),ai=function(t){return{toPeriodicOscSpec:function(e){return vr()({reflectSymbol:function(){return"realImg"}})(b.value)({real:To(e.value0),img:To(e.value1)})}}};var Up={toInitializeTriangleOsc:function(t){return Rg(function(e){return{frequency:e}}(t))}};var Yg={toInitializeStereoPanner:function(t){return Ng(function(e){return{pan:e}}(t))}};var Hp={toInitializeSquareOsc:function(t){return Lg(function(e){return{frequency:e}}(t))}};var Hi={toInitializeSinOsc:function(t){return Wg(function(e){return{frequency:e}}(t))}};var Zg={toInitializeSawtoothOsc:function(t){return qg(function(e){return{frequency:e}}(t))}};var jv={toInitializeRecorder:function(t){return Gg(function(e){return{cb:e}}(t))}};var t_={toInitializeMicrophone:function(t){return Bg(function(e){return{microphone:e}}(t))}};var tE=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(u){return{feedforward:bf(a)(Da()(r.value0)),feedback:bf(a)(Da()(r.value1))}}}}}}};var _t={toInitializeGain:function(t){return Ug(function(e){return{gain:e}}(t))}};var eE={toInitializeConvolver:function(t){return Hg(function(e){return{buffer:e}}(t))}},rE={toInitializeConstant:function(t){return zg(function(e){return{offset:e}}(t))}};var CM={convertOption:function(t){return function(e){return K(Y)}}},e_={convertOption:function(t){return function(e){return K(Y)}}},nE={convertOption:function(t){return function(e){return K(Y)}}},aE={convertOption:function(t){return function(e){return Dr}}},uE={convertOption:function(t){return function(e){return K(Y)}}},ui={convertOption:function(t){return function(e){return K(Y)}}},vc={convertOption:function(t){return function(e){return K(Y)}}},Dc={convertOption:function(t){return function(e){return K(Y)}}},dc={convertOption:function(t){return function(e){return K(Y)}}},bc={convertOption:function(t){return function(e){return K(Y)}}},yc={convertOption:function(t){return function(e){return K(Y)}}},oE={convertOption:function(t){return function(e){return K(Y)}}},iE={convertOption:function(t){return function(e){return K(Y)}}},fE={convertOption:function(t){return function(e){return K(Y)}}},Xv={convertOption:function(t){return function(e){return K(Y)}}},Tf={convertOption:function(t){return function(e){return K(Y)}}},r_={convertOption:function(t){return function(e){return K(Y)}}},n_={convertOption:function(t){return function(e){return K(Y)}}};var Ac={convertOption:function(t){return function(e){return K(Y)}}},cE={convertOption:function(t){return function(e){return K(Y)}}},lE={convertOption:function(t){return function(e){return K(Y)}}},_E={convertOption:function(t){return function(e){return K(Y)}}},Qv={convertOption:function(t){return function(e){return K(Y)}}};var pE={convertOption:function(t){return function(e){return K(Y)}}},Kv={convertOption:function(t){return function(e){return K(Y)}}},_n={convertOption:function(t){return function(e){return K(Y)}}},nn={convertOption:function(t){return function(e){return K(Y)}}},Yv={convertOption:function(t){return function(e){return K(Y)}}},zp={convertOption:function(t){return function(e){return K(Y)}}},hM=function(t){return t.toPeriodicOscSpec},oi=function(t){return{convertOption:function(e){return function(r){return hM(t)}}}},Zv=function(t){return t.toInitializeWaveShaper},sE=function(t){return t.toInitializeTriangleOsc},mE=function(t){return t.toInitializeStereoPanner},vE=function(t){return t.toInitializeSquareOsc},DE=function(t){return t.toInitializeSinOsc},dE=function(t){return t.toInitializeSawtoothOsc},bE=function(t){return t.toInitializeRecorder},tD=function(t){return t.toInitializePlayBuf},yE=function(t){return t.toInitializePeriodicOsc},AE=function(t){return t.toInitializePeaking},kE=function(t){return t.toInitializeNotch},gE=function(t){return t.toInitializeMicrophone},EE=function(t){return t.toInitializeLowshelf},eD=function(t){return t.toInitializeLowpass},rD=function(t){return t.toInitializeLoopBuf},CE=function(t){return t.toInitializeIIRFilter},hE=function(t){return t.toInitializeHighshelf},nD=function(t){return t.toInitializeHighpass},SE=function(t){return t.toInitializeGain},TE=function(t){return t.toInitializeDynamicsCompressor},aD=function(t){return t.toInitializeDelay},xE=function(t){return t.toInitializeConvolver},FE=function(t){return t.toInitializeConstant},uD=function(t){return t.toInitializeBandpass},oD=function(t){return t.toInitializeAllpass},SM={oversample:Kg},TM=function(t){return{toInitializeWaveShaper:function(e){return ia(t)(lM.value)(SM)(e)}}},$E={toInitializeWaveShaper:function(){var t=Zv(TM(At(yt()(H(bt)(CM)()()()({reflectSymbol:function(){return"curve"}})))(dt()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},xM={bufferOffset:0,playbackRate:1,duration:Np},a_=function(t){return{toInitializePlayBuf:function(e){return ia(t)(_M.value)(xM)(e)}}},Na={toInitializePlayBuf:function(){var t=tD(a_(At(yt()(H(bt)(e_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},FM={},ii=function(t){return{toInitializePeriodicOsc:function(e){return ia(t)(pM.value)(FM)(e)}}},$M={q:1,gain:0},kc=function(t){return{toInitializePeaking:function(e){return ia(t)(sM.value)($M)(e)}}};var wM={q:1},gc=function(t){return{toInitializeNotch:function(e){return ia(t)(mM.value)(wM)(e)}}};var MM={gain:0},wE=function(t){return{toInitializeLowshelf:function(e){return ia(t)(vM.value)(MM)(e)}}};var PM={q:1},iD=function(t){return{toInitializeLowpass:function(e){return ia(t)(DM.value)(PM)(e)}}},ME={toInitializeLowpass:function(){var t=eD(iD(At(yt()(H(bt)(Xv)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},OM={loopStart:0,loopEnd:0,playbackRate:1,duration:Np},xf=function(t){return{toInitializeLoopBuf:function(e){return ia(t)(dM.value)(OM)(e)}}},qe={toInitializeLoopBuf:function(){var t=rD(xf(At(yt()(H(bt)(Tf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},IM={gain:0},PE=function(t){return{toInitializeHighshelf:function(e){return ia(t)(bM.value)(IM)(e)}}};var RM={q:1},fD=function(t){return{toInitializeHighpass:function(e){return ia(t)(yM.value)(RM)(e)}}},Ba={toInitializeHighpass:function(){var t=nD(fD(At(yt()(H(bt)(Qv)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},NM=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),OE=function(t){return{toInitializeDynamicsCompressor:function(e){return ia(t)(AM.value)(NM)(e)}}},LM={maxDelayTime:1},cD=function(t){return{toInitializeDelay:function(e){return ia(t)(kM.value)(LM)(e)}}},Xr={toInitializeDelay:function(){var t=aD(cD(At(yt()(H(bt)(Kv)()()()({reflectSymbol:function(){return"delayTime"}})))(dt()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},WM={q:1},an=function(t){return{toInitializeBandpass:function(e){return ia(t)(gM.value)(WM)(e)}}},lD={toInitializeBandpass:function(){var t=uD(an(At(yt()(H(bt)(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},qM={q:1},Vp=function(t){return{toInitializeAllpass:function(e){return ia(t)(EM.value)(qM)(e)}}},_D={toInitializeAllpass:function(){var t=oD(Vp(At(yt()(H(bt)(zp)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};function pD(t){return()=>t.slice()}function sD(t){return e=>r=>()=>{r[t]=e}}function mD(t){return()=>t.slice()}var Ec=ou,Cc=ou,hc=ou,Ua=ou,Ha=ou,za=ou,Va=ou,Ea=ou;var HM=function(){function t(){}return t.value=new t,t}();var Jp={convertOption:function(t){return function(e){return K(Y)}}},jp={convertOption:function(t){return function(e){return K(Y)}}};var RE=function(t){return function(e){return function(r){return function(n){var u=Zv(t)(r),o=function(c){return function(f){return Pt(function(l){return function(){var m=f.ids();return c.raiseId(m)(),_(C)(J(j)(l(f.deleteFromCache({id:m}))))(Et(kt)(l)(M(O)(q(k)(f.makeWaveShaper({id:m,parent:Dr(c.parent),scope:c.scope,curve:u.curve,oversample:u.oversample})))(ln(m)(f)(je(e)(n)))))()}})}};return o}}}};var zM=function(t){return t.toInitializeAnalyser},VM=function(t){return function(e){return function(r){return Pt(function(n){return function(){var o=r.ids();return n(r.makeSpeaker({id:o}))(),kt(ln(o)(r)(je(t)(e)))(n)()}})}}},Ff=function(t){return VM(t)},vD=function(t){return function(e){return function(r){var n=bE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeRecorder({id:p,parent:Dr(o.parent),scope:o.scope,cb:n.cb})))(ln(p)(c)(je(nr(a)(a)(a))(r)))))()}})}};return u}}},JM=function(t){return function(e){return function(r){return function(n){return function(u){var o=AE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makePeaking({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q,gain:o.gain})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})},gain:function(i){return l.setGain({id:s,gain:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},Sc=function(t){return function(e){return function(r){return function(n){return JM(t)(e)(r)(T(g))(n)}}}},jM=function(t){return function(e){return function(r){return function(n){return function(u){var o=mE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeStereoPanner({id:s,parent:Dr(f.parent),scope:f.scope,pan:o.pan})))(M(O)(_(y)(function(d){return Mr()()()({pan:function(i){return l.setPan({id:s,pan:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},NE=function(t){return function(e){return function(r){return jM(t)(e)(r)(T(g))}}},XM=function(t){return function(e){return function(r){return function(n){return function(u){var o=kE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeNotch({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},Tc=function(t){return function(e){return function(r){return function(n){return XM(t)(e)(r)(T(g))(n)}}}},QM=function(t){return function(e){return function(r){return function(n){return function(u){var o=EE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeLowshelf({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,gain:o.gain})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},gain:function(i){return l.setGain({id:s,gain:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},LE=function(t){return function(e){return function(r){return function(n){return QM(t)(e)(r)(T(g))(n)}}}},WE=function(t){return function(e){return function(r){return function(n){return function(u){var o=eD(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeLowpass({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},Xp=function(t){return function(e){return function(r){return function(n){return WE(t)(e)(r)(T(g))(n)}}}},KM=function(t){return function(e){return function(r){return function(n){var u=function(o){return function(c){return Pt(function(f){return function(){var p=pD(_(Oe)(E(""))(To(r)))(),m=Jr(pv)(g)(Gu(lv)(function(fe){return function(jt){return jt({parent:"@fan@",scope:e(o.scope),raiseId:function(ge){return sD(fe)(ge)(p)}})(c)}})(r)),s=kt(m)(f)(),d=ro(),i=_(C)(ft)(mD(p))(),h=_(_v)(function(fe){return function(jt){return function(ge){return Pt(function(Nr){return function(){return jt.raiseId(fe)(),Nr(ge.connectXToY({from:fe,to:jt.parent}))(),I(tt)(void 0)}})}}})(i),ct=ln(o.parent)(c)(n(h)(function(fe){return fe})),mt=kt(ct)(f)();return Be(C)(xo(mt)(d))(),function(){s(),dn(tt)(!t)(hi(To(i))(function(ge){return f(c.deleteFromCache({id:ge}))}))();var jt=hf(d)(function(ge){if(ge instanceof $t)return ge.value0;if(ge instanceof Ft)return iu(ge.value0);throw new Error("Failed pattern match at WAGS.Control (line 1574, column 36 - line 1576, column 35): "+[ge.constructor.name])})();return jt()}}})}};return u}}}},YM=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(u){return function(o){var c=CE(t)(u)(r)(n),f=function(l){return function(p){return Pt(function(m){return function(){var d=p.ids();return l.raiseId(d)(),_(C)(J(j)(m(p.deleteFromCache({id:d}))))(Et(kt)(m)(M(O)(q(k)(p.makeIIRFilter({id:d,parent:Dr(l.parent),scope:l.scope,feedforward:To(c.feedforward),feedback:To(c.feedback)})))(ln(d)(p)(je(e)(o)))))()}})}};return f}}}}}}}},qE=function(){return function(){return function(t){return function(e){return function(r){return YM()()(e)(t)(b.value)(b.value)}}}}},ZM=function(t){return function(e){return function(r){return function(n){return function(u){var o=hE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeHighshelf({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,gain:o.gain})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},gain:function(i){return l.setGain({id:s,gain:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},GE=function(t){return function(e){return function(r){return function(n){return ZM(t)(e)(r)(T(g))(n)}}}},DD=function(t){return function(e){return function(r){return function(n){return function(u){var o=nD(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeHighpass({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},xc=function(t){return function(e){return function(r){return function(n){return DD(t)(e)(r)(T(g))(n)}}}};var Br=function(t){return function(e){return function(r){return function(n){return function(u){var o=SE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeGain({id:s,parent:Dr(f.parent),scope:f.scope,gain:o.gain})))(M(O)(_(y)(function(d){return Mr()()()({gain:function(i){return l.setGain({id:s,gain:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},re=function(t){return function(e){return function(r){return function(n){return Br(t)(e)(r)(T(g))(n)}}}},Ja=function(t){var e=function(r){return function(n){return Pt(function(u){return function(){var c=ro(),f=t(function(l){return function(p){return Pt(function(m){return function(){return Be(C)(hk(c)(function(d){if(d instanceof Ft)return iu(d.value0);if(d instanceof $t)return dn(tt)(d.value0!==l.parent)(J(j)(l.raiseId(d.value0))(m(n.connectXToY({from:d.value0,to:l.parent}))));throw new Error("Failed pattern match at WAGS.Control (line 1628, column 29 - line 1631, column 104): "+[d.constructor.name])}))(),I(tt)(void 0)}})}});return kt(f({parent:r.parent,scope:r.scope,raiseId:function(l){return function(){return r.raiseId(l)(),Be(C)(xo(l)(c))()}}})(n))(u)()}})}};return e};var tP=function(t){return KM(!1)(K(Y))(t)},Ca=function(t){return function(e){return tP(bk(t))(uo(Ra)(Et(Cf(mr)())(Ea))(e))}},eP=function(t){return function(e){return function(r){return function(n){return function(u){var o=TE(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeDynamicsCompressor({id:s,parent:Dr(f.parent),scope:f.scope,threshold:o.threshold,ratio:o.ratio,knee:o.knee,attack:o.attack,release:o.release})))(M(O)(_(y)(function(d){return Mr()()()({threshold:function(i){return l.setThreshold({id:s,threshold:i})},ratio:function(i){return l.setRatio({id:s,ratio:i})},knee:function(i){return l.setKnee({id:s,knee:i})},attack:function(i){return l.setAttack({id:s,attack:i})},release:function(i){return l.setRelease({id:s,release:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},BE=function(t){return function(e){return function(r){return eP(t)(e)(r)(T(g))}}},u_=function(t){return function(e){return function(r){return function(n){return function(u){var o=aD(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeDelay({id:s,parent:Dr(f.parent),scope:f.scope,delayTime:o.delayTime,maxDelayTime:o.maxDelayTime})))(M(O)(_(y)(function(d){return Mr()()()({delayTime:function(i){return l.setDelay({id:s,delayTime:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},oo=function(t){return function(e){return function(r){return function(n){return u_(t)(e)(r)(T(g))(n)}}}};var rP=function(){return{cb:function(t){return I(tt)(I(tt)(void 0))},fftSize:Uv.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:zv.value,channelInterpretation:Hv.value}}(),Qp=function(t){return{toInitializeAnalyser:function(e){return ia(t)(HM.value)(rP)(e)}}};var UE=function(t){return function(e){return function(r){return function(n){var u=xE(t)(r),o=function(c){return function(f){return Pt(function(l){return function(){var m=f.ids();return c.raiseId(m)(),_(C)(J(j)(l(f.deleteFromCache({id:m}))))(Et(kt)(l)(M(O)(q(k)(f.makeConvolver({id:m,parent:Dr(c.parent),scope:c.scope,buffer:u.buffer})))(ln(m)(f)(je(e)(n)))))()}})}};return o}}}},dD=function(t){return function(e){return function(r){return function(n){return function(u){var o=uD(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeBandpass({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},pn=function(t){return function(e){return function(r){return function(n){return dD(t)(e)(r)(T(g))(n)}}}},nP=function(t){return function(e){return function(r){return function(n){return function(u){var o=zM(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeAnalyser({id:s,parent:Dr(f.parent),scope:f.scope,cb:o.cb,fftSize:Nm(2)(function(){if(o.fftSize instanceof Gp)return 7;if(o.fftSize instanceof Bp)return 8;if(o.fftSize instanceof Mg)return 9;if(o.fftSize instanceof Pg)return 10;if(o.fftSize instanceof Uv)return 11;if(o.fftSize instanceof Og)return 12;if(o.fftSize instanceof Ig)return 13;throw new Error("Failed pattern match at WAGS.Control (line 191, column 21 - line 198, column 34): "+[o.fftSize.constructor.name])}()),maxDecibels:o.maxDecibels,minDecibels:o.minDecibels,smoothingTimeConstant:o.smoothingTimeConstant,channelCount:o.channelCount,channelCountMode:function(){if(o.channelCountMode instanceof jg)return"explicit";if(o.channelCountMode instanceof zv)return"max";if(o.channelCountMode instanceof Jg)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 204, column 35 - line 207, column 46): "+[o.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(o.channelInterpretation instanceof Hv)return"speakers";if(o.channelInterpretation instanceof Vg)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 208, column 40 - line 210, column 41): "+[o.channelInterpretation.constructor.name])}()})))(M(O)(_(y)(function(d){return Mr()()()({cb:function(i){return l.setAnalyserNodeCb({id:s,cb:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},Kp=function(t){return function(e){return function(r){return nP(t)(e)(r)(T(g))}}},aP=function(t){return function(e){return function(r){return function(n){return function(u){var o=oD(t)(r),c=function(f){return function(l){return Pt(function(p){return function(){var s=l.ids();return f.raiseId(s)(),_(C)(J(j)(p(l.deleteFromCache({id:s}))))(Et(kt)(p)(M(O)(q(k)(l.makeAllpass({id:s,parent:Dr(f.parent),scope:f.scope,frequency:o.frequency,q:o.q})))(M(O)(_(y)(function(d){return Mr()()()({frequency:function(i){return l.setFrequency({id:s,frequency:i})},q:function(i){return l.setQ({id:s,q:i})}})(d)})(n))(ln(s)(l)(je(e)(u))))))()}})}};return c}}}}},o_=function(t){return function(e){return function(r){return function(n){return aP(t)(e)(r)(T(g))(n)}}}},uP=function(t){return function(e){return function(r){var n=sE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeTriangleOsc({id:p,parent:Dr(o.parent),scope:o.scope,frequency:n.frequency})))(_(y)(function(m){return Mr()()()({frequency:function(s){return c.setFrequency({id:p,frequency:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},Yp=function(t){return uP(t)};var oP=function(t){return function(e){return function(r){var n=vE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeSquareOsc({id:p,parent:Dr(o.parent),scope:o.scope,frequency:n.frequency})))(_(y)(function(m){return Mr()()()({frequency:function(s){return c.setFrequency({id:p,frequency:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},bD=function(t){return oP(t)},HE=function(t){return function(e){return bD(t)(e)(T(g))}},iP=function(t){return function(e){return function(r){var n=DE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeSinOsc({id:p,parent:Dr(o.parent),scope:o.scope,frequency:n.frequency})))(_(y)(function(m){return Mr()()()({frequency:function(s){return c.setFrequency({id:p,frequency:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},$f=function(t){return iP(t)},zE=function(t){return function(e){return $f(t)(e)(T(g))}},fP=function(t){return function(e){return function(r){var n=dE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeSawtoothOsc({id:p,parent:Dr(o.parent),scope:o.scope,frequency:n.frequency})))(_(y)(function(m){return Mr()()()({frequency:function(s){return c.setFrequency({id:p,frequency:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},VE=function(t){return fP(t)};var cP=function(t){return function(e){return function(r){var n=tD(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makePlayBuf({id:p,parent:Dr(o.parent),scope:o.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(_(y)(function(m){return Mr()()()({buffer:function(s){return c.setBuffer({id:p,buffer:s})},playbackRate:function(s){return c.setPlaybackRate({id:p,playbackRate:s})},bufferOffset:function(s){return c.setBufferOffset({id:p,bufferOffset:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})},duration:function(s){return c.setDuration({id:p,duration:s})}})(m)})(r))))()}})}};return u}}},Nn=function(t){return cP(t)};var lP=function(t){return function(e){return function(r){var n=yE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makePeriodicOsc({id:p,parent:Dr(o.parent),scope:o.scope,frequency:n.frequency,spec:n.spec})))(_(y)(function(m){return Mr()()()({frequency:function(s){return c.setFrequency({id:p,frequency:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})},spec:function(s){return c.setPeriodicOsc({id:p,spec:s})}})(m)})(r))))()}})}};return u}}},fi=function(t){return lP(t)};var _P=function(t){return function(e){var r=gE(t)(e),n=function(u){return function(o){return Pt(function(c){return function(){var l=o.ids();return u.raiseId(l)(),_(C)(J(j)(c(o.deleteFromCache({id:l}))))(Et(kt)(c)(q(k)(o.makeMicrophone({id:l,parent:Dr(u.parent),scope:u.scope,microphone:r.microphone}))))()}})}};return n}},i_=function(t){return _P(t)};var pP=function(t){return function(e){return function(r){var n=rD(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeLoopBuf({id:p,parent:Dr(o.parent),scope:o.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(_(y)(function(m){return Mr()()()({buffer:function(s){return c.setBuffer({id:p,buffer:s})},playbackRate:function(s){return c.setPlaybackRate({id:p,playbackRate:s})},loopStart:function(s){return c.setLoopStart({id:p,loopStart:s})},loopEnd:function(s){return c.setLoopEnd({id:p,loopEnd:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},he=function(t){return pP(t)};var sP=function(t){return function(e){return function(r){var n=FE(t)(e),u=function(o){return function(c){return Pt(function(f){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(J(j)(f(c.deleteFromCache({id:p}))))(Et(kt)(f)(M(O)(q(k)(c.makeConstant({id:p,parent:Dr(o.parent),scope:o.scope,offset:n.offset})))(_(y)(function(m){return Mr()()()({offset:function(s){return c.setOffset({id:p,offset:s})},onOff:function(s){return c.setOnOff({id:p,onOff:s})}})(m)})(r))))()}})}};return u}}},JE=function(t){return sP(t)};function yD(){window.scrollTo(0,0)}var io=function(t){return t.sequential},hn=function(t){return t.parallel};var sn=function(t){return function(e){return function(r){return V("button")(e)(x(t)(r))}}};var ha=function(){var t={},e="Pure",r="Throw",n="Catch",u="Sync",o="Async",c="Bind",f="Bracket",l="Fork",p="Sequential",m="Map",s="Apply",d="Alt",i="Cons",h="Resume",ct="Release",mt="Finalizer",fe="Finalized",jt="Forked",ge="Fiber",Nr="Thunk";function it(Rt,Ke,Ur,fr){this.tag=Rt,this._1=Ke,this._2=Ur,this._3=fr}function ne(Rt){var Ke=function(Ur,fr,Yt){return new it(Rt,Ur,fr,Yt)};return Ke.tag=Rt,Ke}function se(Rt){return new it(e,void 0)}function It(Rt){try{Rt()}catch(Ke){setTimeout(function(){throw Ke},0)}}function dr(Rt,Ke,Ur){try{return Ke(Ur())}catch(fr){return Rt(fr)}}function Lr(Rt,Ke,Ur){try{return Ke(Ur)()}catch(fr){return Ur(Rt(fr))(),se}}var Wr=function(){var Rt=1024,Ke=0,Ur=0,fr=new Array(Rt),Yt=!1;function gt(){var ar;for(Yt=!0;Ke!==0;)Ke--,ar=fr[Ur],fr[Ur]=void 0,Ur=(Ur+1)%Rt,ar();Yt=!1}return{isDraining:function(){return Yt},enqueue:function(ar){var we,qr;Ke===Rt&&(qr=Yt,gt(),Yt=qr),fr[(Ur+Ke)%Rt]=ar,Ke++,Yt||gt()}}}();function Ro(Rt){var Ke={},Ur=0,fr=0;return{register:function(Yt){var gt=Ur++;Yt.onComplete({rethrow:!0,handler:function(ar){return function(){fr--,delete Ke[gt]}}})(),Ke[gt]=Yt,fr++},isEmpty:function(){return fr===0},killAll:function(Yt,gt){return function(){if(fr===0)return gt();var ar=0,we={};function qr(sr){we[sr]=Ke[sr].kill(Yt,function(Qr){return function(){delete we[sr],ar--,Rt.isLeft(Qr)&&Rt.fromLeft(Qr)&&setTimeout(function(){throw Rt.fromLeft(Qr)},0),ar===0&&gt()}})()}for(var qn in Ke)Ke.hasOwnProperty(qn)&&(ar++,qr(qn));return Ke={},Ur=0,fr=0,function(sr){return new it(u,function(){for(var Qr in we)we.hasOwnProperty(Qr)&&we[Qr]()})}}}}}var Qa=0,un=1,No=2,Wf=3,qf=4,Sr=5,Lo=6;function Gf(Rt,Ke,Ur){var fr=0,Yt=Qa,gt=Ur,ar=null,we=null,qr=null,qn=null,sr=null,Qr=0,ef=0,su=null,Ai=!0;function ki(Zt){for(var ae,Ge,Qe;;)switch(ae=null,Ge=null,Qe=null,Yt){case No:Yt=un;try{gt=qr(gt),qn===null?qr=null:(qr=qn._1,qn=qn._2)}catch(jn){Yt=Sr,ar=Rt.left(jn),gt=null}break;case Wf:Rt.isLeft(gt)?(Yt=Sr,ar=gt,gt=null):qr===null?Yt=Sr:(Yt=No,gt=Rt.fromRight(gt));break;case un:switch(gt.tag){case c:qr&&(qn=new it(i,qr,qn)),qr=gt._2,Yt=un,gt=gt._1;break;case e:qr===null?(Yt=Sr,gt=Rt.right(gt._1)):(Yt=No,gt=gt._1);break;case u:Yt=Wf,gt=dr(Rt.left,Rt.right,gt._1);break;case o:Yt=qf,gt=Lr(Rt.left,gt._1,function(jn){return function(){fr===Zt&&(fr++,Wr.enqueue(function(){fr===Zt+1&&(Yt=Wf,gt=jn,ki(fr))}))}});return;case r:Yt=Sr,ar=Rt.left(gt._1),gt=null;break;case n:qr===null?sr=new it(i,gt,sr,we):sr=new it(i,gt,new it(i,new it(h,qr,qn),sr,we),we),qr=null,qn=null,Yt=un,gt=gt._1;break;case f:Qr++,qr===null?sr=new it(i,gt,sr,we):sr=new it(i,gt,new it(i,new it(h,qr,qn),sr,we),we),qr=null,qn=null,Yt=un,gt=gt._1;break;case l:Yt=Wf,ae=Gf(Rt,Ke,gt._2),Ke&&Ke.register(ae),gt._1&&ae.run(),gt=Rt.right(ae);break;case p:Yt=un,gt=VS(Rt,Ke,gt._1);break}break;case Sr:if(qr=null,qn=null,sr===null)Yt=Lo,gt=we||ar||gt;else switch(ae=sr._3,Qe=sr._1,sr=sr._2,Qe.tag){case n:we&&we!==ae&&Qr===0?Yt=Sr:ar&&(Yt=un,gt=Qe._2(Rt.fromLeft(ar)),ar=null);break;case h:we&&we!==ae&&Qr===0||ar?Yt=Sr:(qr=Qe._1,qn=Qe._2,Yt=No,gt=Rt.fromRight(gt));break;case f:Qr--,ar===null&&(Ge=Rt.fromRight(gt),sr=new it(i,new it(ct,Qe._2,Ge),sr,ae),(we===ae||Qr>0)&&(Yt=un,gt=Qe._3(Ge)));break;case ct:sr=new it(i,new it(fe,gt,ar),sr,we),Yt=un,we&&we!==ae&&Qr===0?gt=Qe._1.killed(Rt.fromLeft(we))(Qe._2):ar?gt=Qe._1.failed(Rt.fromLeft(ar))(Qe._2):gt=Qe._1.completed(Rt.fromRight(gt))(Qe._2),ar=null,Qr++;break;case mt:Qr++,sr=new it(i,new it(fe,gt,ar),sr,we),Yt=un,gt=Qe._1;break;case fe:Qr--,Yt=Sr,gt=Qe._1,ar=Qe._2;break}break;case Lo:for(var zr in su)su.hasOwnProperty(zr)&&(Ai=Ai&&su[zr].rethrow,It(su[zr].handler(gt)));su=null,we&&ar?setTimeout(function(){throw Rt.fromLeft(ar)},0):Rt.isLeft(gt)&&Ai&&setTimeout(function(){if(Ai)throw Rt.fromLeft(gt)},0);return;case Qa:Yt=un;break;case qf:return}}function Hr(Zt){return function(){if(Yt===Lo)return Ai=Ai&&Zt.rethrow,Zt.handler(gt)(),function(){};var ae=ef++;return su=su||{},su[ae]=Zt,function(){su!==null&&delete su[ae]}}}function ce(Zt,ae){return function(){if(Yt===Lo)return ae(Rt.right(void 0))(),function(){};var Ge=Hr({rethrow:!1,handler:function(){return ae(Rt.right(void 0))}})();switch(Yt){case Qa:we=Rt.left(Zt),Yt=Lo,gt=we,ki(fr);break;case qf:we===null&&(we=Rt.left(Zt)),Qr===0&&(Yt===qf&&(sr=new it(i,new it(mt,gt(Zt)),sr,we)),Yt=Sr,gt=null,ar=null,ki(++fr));break;default:we===null&&(we=Rt.left(Zt)),Qr===0&&(Yt=Sr,gt=null,ar=null)}return Ge}}function Pe(Zt){return function(){var ae=Hr({rethrow:!1,handler:Zt})();return Yt===Qa&&ki(fr),ae}}return{kill:ce,join:Pe,onComplete:Hr,isSuspended:function(){return Yt===Qa},run:function(){Yt===Qa&&(Wr.isDraining()?ki(fr):Wr.enqueue(function(){ki(fr)}))}}}function Wo(Rt,Ke,Ur,fr){var Yt=0,gt={},ar=0,we={},qr=new Error("[ParAff] Early exit"),qn=null,sr=t;function Qr(Hr,ce,Pe){var Zt=ce,ae=null,Ge=null,Qe=0,zr={},jn,Xc;t:for(;;)switch(jn=null,Zt.tag){case jt:if(Zt._3===t&&(jn=gt[Zt._1],zr[Qe++]=jn.kill(Hr,function(JS){return function(){Qe--,Qe===0&&Pe(JS)()}})),ae===null)break t;Zt=ae._2,Ge===null?ae=null:(ae=Ge._1,Ge=Ge._2);break;case m:Zt=Zt._2;break;case s:case d:ae&&(Ge=new it(i,ae,Ge)),ae=Zt,Zt=Zt._1;break}if(Qe===0)Pe(Rt.right(void 0))();else for(Xc=0,jn=Qe;Xc<jn;Xc++)zr[Xc]=zr[Xc]();return zr}function ef(Hr,ce,Pe){var Zt,ae,Ge,Qe,zr,jn;Rt.isLeft(Hr)?(Zt=Hr,ae=null):(ae=Hr,Zt=null);t:for(;;){if(Ge=null,Qe=null,zr=null,jn=null,qn!==null)return;if(ce===null){fr(Zt||ae)();return}if(ce._3!==t)return;switch(ce.tag){case m:Zt===null?(ce._3=Rt.right(ce._1(Rt.fromRight(ae))),ae=ce._3):ce._3=Zt;break;case s:if(Ge=ce._1._3,Qe=ce._2._3,Zt){if(ce._3=Zt,zr=!0,jn=ar++,we[jn]=Qr(qr,Zt===Ge?ce._2:ce._1,function(){return function(){delete we[jn],zr?zr=!1:Pe===null?ef(Zt,null,null):ef(Zt,Pe._1,Pe._2)}}),zr){zr=!1;return}}else{if(Ge===t||Qe===t)return;ae=Rt.right(Rt.fromRight(Ge)(Rt.fromRight(Qe))),ce._3=ae}break;case d:if(Ge=ce._1._3,Qe=ce._2._3,Ge===t&&Rt.isLeft(Qe)||Qe===t&&Rt.isLeft(Ge))return;if(Ge!==t&&Rt.isLeft(Ge)&&Qe!==t&&Rt.isLeft(Qe))Zt=ae===Ge?Qe:Ge,ae=null,ce._3=Zt;else if(ce._3=ae,zr=!0,jn=ar++,we[jn]=Qr(qr,ae===Ge?ce._2:ce._1,function(){return function(){delete we[jn],zr?zr=!1:Pe===null?ef(ae,null,null):ef(ae,Pe._1,Pe._2)}}),zr){zr=!1;return}break}Pe===null?ce=null:(ce=Pe._1,Pe=Pe._2)}}function su(Hr){return function(ce){return function(){delete gt[Hr._1],Hr._3=ce,ef(ce,Hr._2._1,Hr._2._2)}}}function Ai(){var Hr=un,ce=Ur,Pe=null,Zt=null,ae,Ge;t:for(;;)switch(ae=null,Ge=null,Hr){case un:switch(ce.tag){case m:Pe&&(Zt=new it(i,Pe,Zt)),Pe=new it(m,ce._1,t,t),ce=ce._2;break;case s:Pe&&(Zt=new it(i,Pe,Zt)),Pe=new it(s,t,ce._2,t),ce=ce._1;break;case d:Pe&&(Zt=new it(i,Pe,Zt)),Pe=new it(d,t,ce._2,t),ce=ce._1;break;default:Ge=Yt++,Hr=Sr,ae=ce,ce=new it(jt,Ge,new it(i,Pe,Zt),t),ae=Gf(Rt,Ke,ae),ae.onComplete({rethrow:!1,handler:su(ce)})(),gt[Ge]=ae,Ke&&Ke.register(ae)}break;case Sr:if(Pe===null)break t;Pe._1===t?(Pe._1=ce,Hr=un,ce=Pe._2,Pe._2=t):(Pe._2=ce,ce=Pe,Zt===null?Pe=null:(Pe=Zt._1,Zt=Zt._2))}for(sr=ce,Ge=0;Ge<Yt;Ge++)gt[Ge].run()}function ki(Hr,ce){qn=Rt.left(Hr);var Pe;for(var Zt in we)if(we.hasOwnProperty(Zt)){Pe=we[Zt];for(Zt in Pe)Pe.hasOwnProperty(Zt)&&Pe[Zt]()}we=null;var ae=Qr(Hr,sr,ce);return function(Ge){return new it(o,function(Qe){return function(){for(var zr in ae)ae.hasOwnProperty(zr)&&ae[zr]();return se}})}}return Ai(),function(Hr){return new it(o,function(ce){return function(){return ki(Hr,ce)}})}}function VS(Rt,Ke,Ur){return new it(o,function(fr){return function(){return Wo(Rt,Ke,Ur,fr)}})}return it.EMPTY=t,it.Pure=ne(e),it.Throw=ne(r),it.Catch=ne(n),it.Sync=ne(u),it.Async=ne(o),it.Bind=ne(c),it.Bracket=ne(f),it.Fork=ne(l),it.Seq=ne(p),it.ParMap=ne(m),it.ParApply=ne(s),it.ParAlt=ne(d),it.Fiber=Gf,it.Supervisor=Ro,it.Scheduler=Wr,it.nonCanceler=se,it}(),XE=ha.Pure,gP=ha.Throw;function QE(t){return function(e){return e.tag===ha.Pure.tag?ha.Pure(t(e._1)):ha.Bind(e,function(r){return ha.Pure(t(r))})}}function KE(t){return function(e){return ha.Bind(t,e)}}var YE=ha.Sync;function ZE(t){return function(e){return ha.ParMap(t,e)}}function tC(t){return function(e){return ha.ParApply(t,e)}}function eC(t){return function(e){return ha.ParAlt(t,e)}}var Fc=ha.Async;function rC(t,e){return function(){return ha.Fiber(t,null,e)}}var EP=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return ha.Async(function(u){return function(){var o=t(n,u(r()));return function(){return ha.Sync(function(){return r(e(n,o))})}}})}}(),nC=ha.Seq;var hP=function(t){return function(e){return function(r){var n=io(t),u=lr(t.Applicative1())(e)(function(){var o=hn(t);return function(c){return o(r(c))}}());return function(o){return n(u(o))}}}},aC=function(t){return function(e){return function(r){var n=io(t),u=Un(e)(t.Applicative1())(function(){var o=hn(t);return function(c){return o(r(c))}}());return function(o){return n(u(o))}}}},uC=function(t){return function(e){return hP(t)(e)(K(Y))}};var SP=function(t){return t};var iC=function(t){return t};var c_=function(t){return t.toDuration};var fC={fromDuration:em()()(SP)(function(t){return t*1e3}),toDuration:em()()(iC)(function(t){return t/1e3})};var cC=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}};var xP=function(t){return t};var wc={map:ZE},ci={map:QE};var FP=function(){var t=function(n){if(n instanceof $t)return n.value0;if(n instanceof Ft)return uu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof Ft)return n.value0;if(n instanceof $t)return uu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof Ft)return!0;if(n instanceof $t)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:Ft.create,right:$t.create}}(),$P=function(t){return rC(FP,t)},fo=function(t){return function(){var r=$P(t)();return r.run(),r}},wo=function(){var t=Be(C);return function(e){return t(fo(e))}}();var li={apply:tC,Functor0:function(){return wc}};var AD={Applicative0:function(){return fa},Bind1:function(){return Or}},Or={bind:KE,Apply0:function(){return kD(0)}},fa={pure:XE,Apply0:function(){return kD(0)}},kD=cC("applyAff","Effect.Aff",function(){return{apply:Ou(AD),Functor0:function(){return ci}}}),lC=kD(71);var Pr={liftEffect:YE,Monad0:function(){return AD}},_C=function(){var t=_r(Pr);return function(e){return xP(E(t(e)))}}(),pC=function(t){return Fc(function(e){return _(C)(_C)(t.join(e))})};var sC=function(t){return function(e){return rt(Or)(_r(Pr)(e.isSuspended))(function(r){return r?_r(Pr)(Be(C)(e.kill(t,E(I(tt)(void 0))))):Fc(function(n){return _(C)(_C)(e.kill(t,n))})})}};var Sn={parallel:ft,sequential:nC,Monad0:function(){return AD},Applicative1:function(){return wP(0)}},wP=cC("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=hn(Sn),e=I(fa);return function(r){return t(e(r))}}(),Apply0:function(){return li}}});var MP={append:function(t){return function(e){return function(r){return uC(Sn)(Jt)([t(r),e(r)])}}}};var PP=E(I(fa)(void 0)),mC={mempty:PP,Semigroup0:function(){return MP}};var vC={alt:eC,Functor0:function(){return wc}};var DC=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),l_=function(){function t(){}return t.value=new t,t}(),wf=function(){function t(){}return t.value=new t,t}(),__=function(){function t(){}return t.value=new t,t}(),Mf=function(){function t(){}return t.value=new t,t}(),p_=function(){function t(){}return t.value=new t,t}(),dC=function(){function t(){}return t.value=new t,t}(),Zp=function(){function t(){}return t.value=new t,t}(),ts=function(){function t(){}return t.value=new t,t}(),s_=function(){function t(){}return t.value=new t,t}(),m_=function(){function t(){}return t.value=new t,t}(),bC=function(){function t(){}return t.value=new t,t}(),Mc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),gD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var OP=function(t){for(var e="",r="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",n=r.length,u=0;u<t;u++)e+=r.charAt(Math.floor(Math.random()*n));return e},IP="numeric",RP="sudden",NP="cancellation",LP="step",WP="linear",qP="exponential",GP="envelope";var BP=function(t){return t.type===NP},kC=function(t,e,r){if(r.type===RP)t.value=r.value.n;else if(r.type===IP)t[r.value.t.type===LP?"setValueAtTime":r.value.t.type===WP?"linearRampToValueAtTime":r.value.t.type===qP?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,e+r.value.o);else if(BP(r))r.value.hold?t.cancelAndHoldAtTime(e+r.value.o):t.cancelScheduledValues(e+r.value.o);else if(r.type===GP){let n=e+r.value.o;t.cancelScheduledValues(Math.max(0,n)),t.setValueCurveAtTime(r.value.p,n,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},UP=function(t,e,r,n){return kC(t.parameters.get(e),r,n)},Tu=function(t,e,r,n){return kC(t[e],r,n)},yr=function(t,e,r){r.scopes[e.value]||(r.scopes[e.value]=[]),r.scopes[e.value].push(t),r.units[t].scope=e},Ar=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},kr=function(t,e,r){e.type==="just"&&gC(t,e.value,r)},gC=function(t,e,r){var n=function(){r.units[t].outgoing.push(e),r.units[e].incoming.push(t),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var u={f:n};e!==t&&!r.units[e]&&(u.w=e),r.toConnect[t].push(u);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var u={f:n};e!==t&&!r.units[t]&&(u.w=t),r.toConnect[e].push(u);return}n()};function ED(t){return function(e){return function(){delete e.units[t.id]}}}function CD(t){return function(e){return function(){gC(t.from,t.to,e)}}}var v_=function(t){return function(e){return function(r){return function(){if(r.units[t].outgoing=r.units[t].outgoing.filter(function(u){return u!==e}),r.units[e].incoming=r.units[e].incoming.filter(function(u){return u!==t}),r.units[t].main.disconnect(r.units[e].main),r.units[e].se&&r.units[t].main.disconnect(r.units[e].se),r.units[ptr].scope==="@fan@")return;let n=r.units[ptr].scope;r.scopes[n].forEach(u=>{delete r.units[u]}),delete r.scopes[n]}}}};function v_(t){return function(e){return function(){return v_(t.from)(t.to)(e)()}}}function hD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:t.q,frequency:t.frequency})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function SD(t){return function(e){return function(){var r=t.id,n=t.cb,u=new AnalyserNode(e.context,t),o=n(u)();e.units[r]={outgoing:[],incoming:[],analyserOrig:n,analyser:o,main:e.context.createGain(),se:u},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function TD(t){return function(e){return function(){var r=t.id,n=t.options;e.units[r]={outgoing:[],incoming:[],main:new AudioWorkletNode(e.context,n.name,{numberOfInputs:n.numberOfInputs,numberOfOutputs:n.numberOfOutputs,outputChannelCount:n.outputChannelCount,parameterData:n.parameterData,processorOptions:n.processorOptions})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function xD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:t.q,frequency:t.frequency})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function FD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new ConstantSourceNode(o,c)},u={offset:t.offset};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function $D(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[],incoming:[],main:new ConvolverNode(e.context,{buffer:t.buffer})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function wD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new DelayNode(e.context,{delayTime:t.delayTime,maxDelayTime:t.maxDelayTime})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function MD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new DynamicsCompressorNode(e.context,{knee:t.knee,ratio:t.ratio,threshold:t.threshold,attack:t.attack,release:t.release})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}var PD=function(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new GainNode(e.context,{gain:t.gain})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}};function OD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:t.q,frequency:t.frequency})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function ID(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:t.frequency,gain:t.gain})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function RD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[],incoming:[],main:new IIRFilterNode(e.context,{feedforward:t.feedforward,feedback:t.feedback})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function ND(t){return function(e){return function(){var r=t.id,n=function(o,c){return new AudioBufferSourceNode(o,c)},u={loop:!0,buffer:t.buffer,loopStart:t.loopStart,loopEnd:t.loopEnd,playbackRate:t.playbackRate};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function LD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:t.q,frequency:t.frequency})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function WD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:t.frequency,gain:t.gain})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function qD(t){return function(e){return function(){var r=t.id,n=t.element,u=function(){var o=e.context.createMediaElementSource(n);return o};e.units[r]={outgoing:[t.parent],incoming:[],createClosure:u,resumeClosure:{},main:u()},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function GD(t){return function(e){return function(){var r=t.id;e.units[t.id]={main:e.context.createMediaStreamSource(t.microphone),outgoing:[t.parent],incoming:[]},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function BD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:t.frequency,Q:t.q})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function UD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:t.frequency,Q:t.q,gain:t.gain})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function HD(t){return function(e){return function(){var r=t.id,n=function(o,c){var f={frequency:c.frequency,periodicWave:c.spec.type==="wave"?c.spec.value:xd(e.context)(c.spec.value.real)(c.spec.value.img)()},l=new OscillatorNode(o,f);return l},u={frequency:t.frequency,type:"custom",spec:t.spec};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function zD(t){return function(e){return function(){var r=t.id,n=function(o,c){var f={loop:c.loop,buffer:c.buffer,playbackRate:c.playbackRate};return new AudioBufferSourceNode(o,f)},u={loop:!1,buffer:t.buffer,playbackRate:t.playbackRate,bufferOffset:t.bufferOffset,duration:t.duration};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function VD(t){return function(e){return function(){var r=t.id,n=t.cb,u=e.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);n(o)(),o.start(),e.units[r]={outgoing:[],incoming:[],recorderOrig:n,recorder:o,main:e.context.createGain(),se:u},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function JD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"sawtooth"};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function jD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"sine"};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function XD(t){return function(e){return function(){e.units[t.id]={outgoing:[],incoming:[],main:e.context.createGain(),se:e.context.destination}}}}function QD(t){return function(e){return function(){var r=t.id;e.units[r]={outgoing:[t.parent],incoming:[],main:new StereoPannerNode(e.context,{pan:t.pan})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function KD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"square"};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function YD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"triangle"};e.units[r]={outgoing:[],incoming:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function ZD(t){return function(e){return function(){var r=t.id,n=t.curve,u=t.oversample;e.units[r]={outgoing:[n.parent],incoming:[],main:new WaveShaperNode(e.context,{curve:n,oversample:u.type})},yr(r,t.scope,e),Ar(r,e),kr(r,t.parent,e)}}}function td(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function ed(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var u=r;e.units[n].recorderOrig=r;var o=new MediaRecorder(e.units[n].se);u(o)(),o.start()}}}}function rd(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function nd(t){return function(e){return function(){var r=t.id,n=t.paramName,u=t.paramValue;UP(e.units[r].main,n,e.deprecatedWriteHead,u)}}}var xu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function ad(t){return function(e){return function(){var r=t.id,n=t.gain;Tu(e.units[r].main,"gain",e.deprecatedWriteHead,n),xu(n,e.units[r],"gain")}}}function ud(t){return function(e){return function(){var r=t.id,n=t.q;Tu(e.units[r].main,"Q",e.deprecatedWriteHead,n),xu(n,e.units[r],"Q")}}}function od(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function id(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function fd(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function cd(t){return function(e){return function(){var r=t.id,n=t.pan;Tu(e.units[r].main,"pan",e.deprecatedWriteHead,n),xu(n,e.units[r],"pan")}}}function ld(t){return function(e){return function(){var r=t.id,n=t.threshold;Tu(e.units[r].main,"threshold",e.deprecatedWriteHead,n),xu(n,e.units[r],"threshold")}}}function _d(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function pd(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function sd(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function md(t){return function(e){return function(){var r=t.id,n=t.duration;e.units[r].duration=n}}}function vd(t){return function(e){return function(){var r=t.id,n=t.release;Tu(e.units[r].main,"release",e.deprecatedWriteHead,n),xu(n,e.units[r],"release")}}}function Dd(t){return function(e){return function(){var r=t.id,n=t.offset;Tu(e.units[r].main,"offset",e.deprecatedWriteHead,n),xu(n,e.units[r],"offset")}}}function dd(t){return function(e){return function(){var r=t.id,n=t.ratio;Tu(e.units[r].main,"ratio",e.deprecatedWriteHead,n),xu(n,e.units[r],"ratio")}}}function bd(t){return function(e){return function(){var r=t.id,n=t.attack;Tu(e.units[r].main,"attack",e.deprecatedWriteHead,n),xu(n,e.units[r],"attack")}}}function yd(t){return function(e){return function(){var r=t.id,n=t.knee;Tu(e.units[r].main,"knee",e.deprecatedWriteHead,n),xu(n,e.units[r],"knee")}}}function Ad(t){return function(e){return function(){var r=t.id,n=t.delayTime;Tu(e.units[r].main,"delayTime",e.deprecatedWriteHead,n),xu(n,e.units[r],"delayTime")}}}function kd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;Tu(e.units[r].main,"playbackRate",e.deprecatedWriteHead,n),xu(n,e.units[r],"playbackRate")}}}function gd(t){return function(e){return function(){var r=t.id,n=t.frequency;Tu(e.units[r].main,"frequency",e.deprecatedWriteHead,n),xu(n,e.units[r],"frequency")}}}function Ed(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?yC(r)(n)(e)():n.x.type==="off"?AC(r)(n)(e)():n.x.type==="offOn"&&(AC(r)({x:{type:"off"},o:0})(e)(),yC(r)({x:{type:"on"},o:n.o})(e)())}}}var yC=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].outgoing.length;n++){var u=r.units[t].outgoing[n];r.units[t].main.connect(r.units[u].main),r.units[u].se&&r.units[t].main.connect(r.units[u].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?r.units[t].resume.duration.type==="just"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration.value):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},AC=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main,u=r.units[t].outgoing.slice();n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function Cd(t){return function(){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}}function es(t){return function(){t.stop()}}function hd(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(u){n.push(u.data)},r.onstop=function(){var u=new Blob(n,{type:t});e(u)(),n=null}}}}}function Sd(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function D_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function Td(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var xd=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),u=new Float32Array(r.length),o=0;o<e.length;o++)n[o]=e[o];for(var o=0;o<r.length;o++)u[o]=r[o];return t.createPeriodicWave(n,u,{disableNormalization:!0})}}}};function zi(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},unqidfr:OP(10),scopes:{},unsu:{},toConnect:{}}}}function Fd(t){return function(){t.close()}}function $d(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function wd(t){return function(e){return function(){return t.decodeAudioData(e)}}}function Md(){return new(window.AudioContext||window.webkitAudioContext)}function Pd(t){return function(){return t.state}}function d_(t){return function(){return t.currentTime}}function EC(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var VP=function(t){return function(e){return Fc(function(r){return S_(C)(be(mC))(EC(e)(function(n){return r(Ft.create(t(n)))()})(function(n){return r($t.create(n))()}))})}};var JP=function(t){return Fa(function(e){return ho("Promise failed, couldn't extract JS Error or String")})(K(Y))(Pv(M(Mv(ym)(Nu))(qv(Nu)("Error")(t))(_(Kl(vo))(ho)(Gv(Nu)(t)))))},CC=VP(JP),rs=function(t){return rt(Or)(_r(Pr)(t))(CC)};function Od(t){return function(){return URL.createObjectURL(t)}}var hC=function(t){return function(e){return function(r){return Et(hd(t))(r)(function(){var n=Gn(Le)(e);return function(u){return n(Od(u))}}())}}};var Pf={ids:_(C)(Ht(uf))(Ga),deleteFromCache:ED,disconnectXFromY:v_,connectXToY:CD,makeAllpass:hD,makeAnalyser:SD,makeAudioWorkletNode:TD,makeBandpass:xD,makeConstant:FD,makeConvolver:$D,makeDelay:wD,makeDynamicsCompressor:MD,makeGain:PD,makeHighpass:OD,makeHighshelf:ID,makeIIRFilter:RD,makeLoopBuf:ND,makeLowpass:LD,makeLowshelf:WD,makeMediaElement:qD,makeMicrophone:GD,makeNotch:BD,makePeaking:UD,makePeriodicOsc:HD,makePlayBuf:zD,makeRecorder:VD,makeSawtoothOsc:JD,makeSinOsc:jD,makeSpeaker:XD,setDuration:md,makeSquareOsc:KD,makeStereoPanner:QD,makeTriangleOsc:YD,makeWaveShaper:ZD,setAnalyserNodeCb:td,setMediaRecorderCb:ed,setWaveShaperCurve:rd,setAudioWorkletParameter:nd,setBuffer:od,setConvolverBuffer:id,setPeriodicOsc:fd,setOnOff:Ed,setBufferOffset:sd,setLoopStart:_d,setLoopEnd:pd,setRatio:dd,setOffset:Dd,setAttack:bd,setGain:ad,setQ:ud,setPan:cd,setThreshold:ld,setRelease:vd,setKnee:yd,setDelay:Ad,setPlaybackRate:kd,setFrequency:gd},wt=function(t){return function(e){return rt(Or)(rs($d(e)))(function(){var r=wd(t);return function(n){return rs(r(n))}}())}},b_=function(t){var e=_r(t);return function(r){return e(Pd(r))}};var Jn=function(t){return _r(t)(Md)},Fu=function(t){var e=_r(t);return function(r){return e(Td(r))}},mn=function(t){return function(e){return _r(t)(function(){var n=b_(or)(e)();return dn(tt)(n!=="closed")(Fd(e))()})}},YP=ft,ZP=ft,ns=function(t){return function(e){return _(ci)(function(r){return{microphone:function(){return t?I(mo)(YP(r)):z.value}(),camera:function(){return e?I(mo)(ZP(r)):z.value}()}})(rs(Sd(t)(e)))}};var Mo=function(){function t(){}return t.value=new t,t}(),Po=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),pu=function(){function t(){}return t.value=new t,t}(),Tn=yD,_i=function(t){return io(Sn)(M(vC)(hn(Sn)(rt(Or)(pC(t))(_r(Pr))))(hn(Sn)(sC(ho("We navigated away from the page"))(t))))},Pc=function(t){return function(e){return function(r){return function(n){return M(t)(q(e)(pu.value))(n)}}}},ja=function(t){return function(e){return function(r){return function(n){return M(t)(q(e)(Q(pr)(cr.value)(Ze(E(n)))))(_(t.Functor0())(function(u){return Q(pr)(cr.value)(Ze(E(J(j)(u)(n))))})(_(t.Functor0())(function(u){return u.value0})(r)))}}}},as=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return function(l){return _(t)(function(p){return Q(pr)(cr.value)(Ze(E(function(){if(p.value0 instanceof Mo)return I(tt)(void 0);if(p.value0 instanceof Po)return J(j)(J(j)(p.value0.value0)(n(I(tt)(void 0))))(u(pu.value));if(p.value0 instanceof pu)return function(){p.value1(),u(Mo.value)();var s=fo(rt(Or)(Jn(Pr))(function(d){return rt(Or)(Fu(Pr)(d))(function(i){return rt(Or)(o(d))(function(h){return _r(Pr)(function(){var mt=c(d)(h)(),fe=J(j)(J(j)(mt)(i))(mn(or)(d));return u(new Po(fe))(),fe})})})}))();return Uf(e)(Le)(n(function(){return u(pu.value)(),wo(_i(s))()}))(function(){return I(tt)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[p.value0.constructor.name])}())))})(En(r)(M(r.Plus0().Alt0())(q(r)(I(tt)(void 0)))(_(t)(function(p){return p.value0})(f)))(_(t)(et.create)(l)))}}}}}}}}},Sa=function(t){return function(e){return function(r){return function(){return t(r)(),e(new DC(r))()}}}},us=function(t){return function(e){return function(r){return function(n){return function(u){return on(function(o){return function(c){var f=Pc(O)(k)(e)(c);return x(N(a)(a))(sc(Ee(a)(a))(M(O)(q(k)(Q(Ip)(qt.value)("cursor: pointer;")))(as(y)(Go)(k)(r)(o)(n)(u)(e)(f)))([rn(_(y)(function(l){if(l instanceof pu)return t;if(l instanceof Mo)return"\u23F3";if(l instanceof Po)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[l.constructor.name])})(f))]))}})}}}}},Lt=function(t){return function(e){return function(r){return function(n){return on(function(u){return function(o){var c=Pc(O)(k)(t)(o);return x(N(a)(a))(sn(Ee(a)(a))(as(y)(Go)(k)(e)(u)(r)(n)(t)(c))([rn(_(y)(function(f){if(f instanceof pu)return"Turn on";if(f instanceof Mo)return"Loading...";if(f instanceof Po)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[f.constructor.name])})(c))]))}})}}}};var vn={first:function(t){return function(e){return new et(t(e.value0),e.value1)}},second:_(Uo),Profunctor0:function(){return Ra}},Ln=function(t){return t.second},os=function(t){return t.first};var _O=function(t){return function(e){return function(r){return function(n){return ni(r)(t)(e)(n)}}}};var xC=function(){return function(){return function(t){return _O(Da())(Da())(t)}}};var FC=function(){return function(){return function(t){return xC()()(t)}}};var mO=function(t){return function(e){return function(r){return ni(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(os(e)(r))}}},$C=function(t){return function(e){return function(r){return mO(function(n){return new et(t(n),function(u){return e(n)(u)})})(r)}}};var wC=function(t){return function(){return function(){return function(e){return function(r){return $C(Yo(t)()(e))(Et(JA(t)()()(e)))(r)}}}}};var MC=function(t){return t};var is=function(t){return t};var Rd=function(t){return t};var If={toAudioOnOff:K(Y)};var Oc=function(t){return t.toAudioParameter},PC=function(t){return t.toAudioOnOff};var fs=function(){return MC(function(){var t=FC()()(Ra),e=wC({reflectSymbol:function(){return"o"}})()()(b.value)(vn);return function(r){return t(e(r))}}())},bO=function(){var t=vr()({reflectSymbol:function(){return"sudden"}})(b.value);return function(e){return Rd(t(e))}}();var cs={toAudioParameter:function(t){return bO({n:t})}};var Nd=function(){return vr()({reflectSymbol:function(){return"on"}})(b.value)(void 0)}(),y_={x:Nd,o:0},st=function(){return q(k)(tn()(vr()({reflectSymbol:function(){return"onOff"}})(b.value)(y_)))};var OC=function(){return vr()({reflectSymbol:function(){return"off"}})(b.value)(void 0)}();var yO=function(){var t=vr()({reflectSymbol:function(){return"numeric"}})(b.value);return function(e){return Rd(t(e))}}();var xn={toAudioParameter:yO},Vi=function(){return vr()({reflectSymbol:function(){return"linear"}})(b.value)(void 0)}();var AO=function(){var t=vr()({reflectSymbol:function(){return"envelope"}})(b.value);return function(e){return Rd(t(e))}}();var Ta={toAudioParameter:AO};var Ic=function(t){return function(e){return function(){var n=zi(t)(),u=kt(Ff(Qg(a)(a)(a))(_(y)(je(G(a)(a)(a)))(e))(Pf))(function(o){return o(n)})();return u}}};var St=function(t){return function(e){return function(){var n=zi(t)(),u=kt(Ff(G(a)(a)(a))(e)(Pf))(function(o){return o(n)})();return u}}},ls=function(t){return function(){var r=Jn(or)();return _(C)(function(n){return J(j)(n)(mn(or)(r))})(St(r)(t))()}};var kO=function(){return b.value}(),IC=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
  <h2 id="allpass">Allpass filter</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">all-pass filter</a> <a href="https://en.wikipedia.org/wiki/All-pass_filter">passes through all frequencies of a source at equal volume but changes their phase</a>. Its use by itself is imperceptible, as the human ear (mostly) does not pick up on phase shifts by themselves. However, when an all-pass filter's output is mixed with several chained all-pass filters plus the original source, you hear a neat phaser effect.</p>

  <p>The <code>bangOn</code> is an event that turns the loop buffer on. We'll learn more about turning things on and off in the "Events" section.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> mix $ gain_ 0.2
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
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(b.value)(kO)({allpass:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Ca(he(qe)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(.2)([o,o_(_D)(G(a)(a)(a))(700)([o_(Vp(At(yt()(H(H(bt)(Yv)()()()({reflectSymbol:function(){return"q"}}))(zp)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:990,q:20})([o]),o_(_D)(G(a)(a)(a))(1110)([o,o_(Vp(At(yt()(H(H(bt)(Yv)()()()({reflectSymbol:function(){return"q"}}))(zp)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:2010,q:30})([o])])])]))}})])}}))})}}};function pi(t){return function(r,n,u){if(n===null)return new t(r);var o=r.byteLength,c=t.BYTES_PER_ELEMENT,f=Math.min(o,n>>>0);if(u===null)return new t(r,f);var l=Math.min((o-f)/c,u);return new t(r,f,l)}}var EO=pi(Uint8ClampedArray),CO=pi(Uint32Array),hO=pi(Uint16Array),RC=pi(Uint8Array),SO=pi(Int32Array),TO=pi(Int16Array),xO=pi(Int8Array),FO=pi(Float32Array),$O=pi(Float64Array);function NC(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var _s={create:RC,BinaryValue0:function(){}};var ps=function(t){return function(e){return function(){return NC(e)}}};function ss(t){return t|0}var si=function(){return window};function GC(t,e,r,n){if(typeof window<"u"){var u=window[r];if(u!=null&&n instanceof u)return e(n)}for(var o=n;o!=null;){var c=Object.getPrototypeOf(o),f=c.constructor.name;if(f===r)return e(n);if(f==="Object")return t;o=c}return t}var xt=function(t){return function(e){return GC(z.value,U.create,t,e)}};function BC(t){return function(){return t.body}}var UC=function(){var t=_(C)(jr);return function(e){return t(BC(e))}}();var HC=ft;function Rf(t){return function(){return t.valueAsNumber}}var Rc=xt("HTMLInputElement");function Wd(t){return function(){return t.document}}function ms(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var qd=ft;var vI=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}},Ji=Pt(function(t){return function(){var r=si(),n=ye(!0)(),u=vI("fx","FRP.Event.Animate",function(){return Be(C)(Et(ms)(r)(function(){var f=Ce(n)();return dn(tt)(f)(function(){return t(void 0)(),u(19)()})()}))}),o=u(15);return o(),ze(!1)(n)}});var DI="background-color: rgb(150,30,10);",dI="background-color: rgb(130,60,10);",bI="background-color: rgb(80,90,10);",yI="background-color: rgb(10,130,10);",AI="background-color: rgb(10,100,0);",kI=Sp(qa)(function(t){return ve(pe(vp)()(Ia)()(Rl))(DI)(ve(pe(Vn)()(Gr)()(Ia))(dI)(ve(pe(Aa)()(wr)()(Gr))(bI)(ve(pe(ka)()(hr)()(wr))(yI)(ve(pe(ta)()(oa)()(hr))(AI)(ga)))))}),gI=function(t){return function(e){return function(r){return function(n){return Kp(Qp(At(yt()(H(H(bt)(jp)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(dt()())))(nr(a)(a)(a))({cb:n,fftSize:Bp.value})(he(e)(r)(st()))}}}},EI=function(){return b.value}(),Me="background-color: rgb(255,255,255,0.0);",Re=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return function(l){return function(p){return function(m){return _(t)(function(s){var d=Cf(e)()(Cf(n)()(s)(l))(p);return d?Q(o)(qt.value)(Cf(e)()(Cf(n)()(kI)(l))(p)):Q(o)(qt.value)(Me)})(m)}}}}}}}}}}},CI=function(){return 15/40}(),hI=function(){return 10/40}(),SI=function(){return 7/40}(),TI=function(){return 3/40}(),xI=function(){return 1/40}(),VC=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } (loopBuf atar bangOn)</code></pre>

  ~analyser~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(EI)({analyser:W(at(a)(a))(on(function(n){return function(u){var o=El(lu)(K(Y))(u),c=Pc(O)(k)(r)(function(l){return l.right}(o)),f=function(l){return l.left}(o);return x(N(a)(a))(Je(Ee(a)(a))([sn(Ee(a)(a))(M(O)(q(k)(Q(_c)(qt.value)("cursor: pointer;")))(as(y)(Go)(k)(t)(function(l){return n($t.create(l))})(function(l){return wt(l)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(l){return function(p){return function(){var s=ye(z.value)(),d=zi(l)(),i=Ff(G(a)(a)(a))([gI(Jp)(qe)(p)(function(ct){return function(){return ze(new U(ct))(s)(),ze(z.value)(s)}})])(Pf),h=kt(M(O)(_(y)($t.create)(i))(_(y)(Ft.create)(Ji)))(function(ct){if(ct instanceof $t)return ct.value0(d);if(ct instanceof Ft)return function(){var fe=Ce(s)();return yu(tt)(rr)(fe)(function(jt){return function(){var Nr=D_(jt)(),it=ps(_s)(Nr)(),ne=ye(0)(),se=ye(0)(),It=ye(0)(),dr=ye(0)(),Lr=ye(0)(),Wr=ye(0)(),Ro=ye(0)(),Qa=ye(0)(),un=ye(0)(),No=ye(0)(),Wf=function(Sr){if(Sr<32)return ne;if(Sr<64)return se;if(Sr<96)return It;if(Sr<128)return dr;if(Sr<168)return Lr;if(Sr<160)return Wr;if(Sr<224)return Ro;if(tr)return Qa;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[Sr.constructor.name])};hi(it)(function(Sr){var Lo=ss(Sr);return function(){var Wo=Ce(No)();return Kn(Ue(Bo)(Lo))(un)(),Kn(Ue(Bo)(Lo))(Wf(Wo))(),Kn(Ue(Bo)(1))(No)()}})();var qf=Un(dk)(tt)(function(Sr){return function(){var Gf=_(C)(Ve)(Ce(Sr))(),Wo=_(C)(Lu(ol)(Gf))(_(C)(Ve)(Ce(un)))();return ve(pe(vp)()(Ia)()(Rl))(Wo>CI)(ve(pe(Vn)()(Gr)()(Ia))(Wo>hI)(ve(pe(Aa)()(wr)()(Gr))(Wo>SI)(ve(pe(ka)()(hr)()(wr))(Wo>TI)(ve(pe(ta)()(oa)()(hr))(Wo>xI)(ga)))))}})(ve(pe(QA)()(Km)()(ZA))(ne)(ve(pe(KA)()(Ym)()(Km))(se)(ve(pe(YA)()(Rl)()(Ym))(It)(ve(pe(vp)()(Ia)()(Rl))(dr)(ve(pe(Vn)()(Gr)()(Ia))(Lr)(ve(pe(Aa)()(wr)()(Gr))(Wr)(ve(pe(ka)()(hr)()(wr))(Ro)(ve(pe(ta)()(oa)()(hr))(Qa)(ga)))))))))();return n(new Ft(qf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ct.constructor.name])})();return function(){return h(),function(){var fe=b_(or)(l)();return dn(tt)(fe!=="closed")(mn(or)(l))()}(),n(new Ft(Sp(qa)(E(Sp(ya)(E(!1))))))()}}}})(r)(c)))([rn(_(y)(function(l){if(l instanceof pu)return"Turn on";if(l instanceof Mo)return"Loading...";if(l instanceof Po)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[l.constructor.name])})(c))]),Ae(Ee(a)(a))(q(k)(Q(lt)(qt.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(mr)(Zu)(lt)(ua)(Zu)(Ea)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(Rn)(Yu)(lt)(ua)(Yu)(Va)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(In)(Ku)(lt)(ua)(Ku)(za)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(On)(Qu)(lt)(ua)(Qu)(Ha)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(Pn)(Xu)(lt)(ua)(Xu)(Ua)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(ya)(ju)(lt)(ua)(ju)(hc)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(Co)(Ju)(lt)(ua)(Ju)(Cc)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(mr)(ua)(Eo)(Vu)(lt)(ua)(Vu)(Ec)(Ea)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(mr)(Zu)(lt)(aa)(Zu)(Ea)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(Rn)(Yu)(lt)(aa)(Yu)(Va)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(In)(Ku)(lt)(aa)(Ku)(za)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(On)(Qu)(lt)(aa)(Qu)(Ha)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(Pn)(Xu)(lt)(aa)(Xu)(Ua)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(ya)(ju)(lt)(aa)(ju)(hc)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(Co)(Ju)(lt)(aa)(Ju)(Cc)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Rn)(aa)(Eo)(Vu)(lt)(aa)(Vu)(Ec)(Va)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(mr)(Zu)(lt)(na)(Zu)(Ea)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(Rn)(Yu)(lt)(na)(Yu)(Va)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(In)(Ku)(lt)(na)(Ku)(za)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(On)(Qu)(lt)(na)(Qu)(Ha)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(Pn)(Xu)(lt)(na)(Xu)(Ua)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(ya)(ju)(lt)(na)(ju)(hc)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(Co)(Ju)(lt)(na)(Ju)(Cc)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(In)(na)(Eo)(Vu)(lt)(na)(Vu)(Ec)(za)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(mr)(Zu)(lt)(ra)(Zu)(Ea)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(Rn)(Yu)(lt)(ra)(Yu)(Va)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(In)(Ku)(lt)(ra)(Ku)(za)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(On)(Qu)(lt)(ra)(Qu)(Ha)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(Pn)(Xu)(lt)(ra)(Xu)(Ua)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(ya)(ju)(lt)(ra)(ju)(hc)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(Co)(Ju)(lt)(ra)(Ju)(Cc)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(On)(ra)(Eo)(Vu)(lt)(ra)(Vu)(Ec)(Ha)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(mr)(Zu)(lt)(ea)(Zu)(Ea)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(Rn)(Yu)(lt)(ea)(Yu)(Va)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(In)(Ku)(lt)(ea)(Ku)(za)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(On)(Qu)(lt)(ea)(Qu)(Ha)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(Pn)(Xu)(lt)(ea)(Xu)(Ua)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(ya)(ju)(lt)(ea)(ju)(hc)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(Co)(Ju)(lt)(ea)(Ju)(Cc)(Ua)(f)))(ee),Ae(N(a)(a))(M(O)(q(k)(Q(lt)(qt.value)(Me)))(Re(y)(Pn)(ea)(Eo)(Vu)(lt)(ea)(Vu)(Ec)(Ua)(f)))(ee)])]))}}))})}}};var $I=function(){return b.value}(),JC=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
  <h2 id="bandpass">Bandpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">bandpass filter</a> isolates a single frequency range of a source. When you crank up a bandpass node's Q value, the isolation gets more intense. At the extreme, the source signal is almost lost and you get a pure sound that resembles a sine-wave oscillator.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
    \\b _ -> mix $ gain_ 0.8
      [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
      , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
      , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
      , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
      , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
      ]
  ]</code></pre>

  @bandpass@
  </section>
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(b.value)($I)({bandpass:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Ca(he(qe)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(.8)([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:400,q:1})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:880,q:5})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:1200,q:10})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:2e3,q:20})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:3e3,q:30})([o])]))}})])}}))})}}};var MI=function(){return b.value}(),jC=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
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
`}})()()(L()(Z)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))(MI)({compression:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([BE(OE(At(yt()(bt))(dt()())))(G(a)(a)(a))({})([he(qe)(u)(st())])])}}))})}}};var bs=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"playbackRate"}})(b.value),n=Oc(t);return function(u){return e(r(n(u)))}}},Nf=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"onOff"}})(b.value),n=PC(t);return function(u){return e(r(n(u)))}}},XC=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"offset"}})(b.value),n=Oc(t);return function(u){return e(r(n(u)))}}},QC=function(){var t=tn(),e=vr()({reflectSymbol:function(){return"loopStart"}})(b.value);return function(r){return t(e(r))}},KC=function(){var t=tn(),e=vr()({reflectSymbol:function(){return"loopEnd"}})(b.value);return function(r){return t(e(r))}},Dn=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"gain"}})(b.value),n=Oc(t);return function(u){return e(r(n(u)))}}},co=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"frequency"}})(b.value),n=Oc(t);return function(u){return e(r(n(u)))}}};var Nc=function(){return function(t){var e=tn(),r=vr()({reflectSymbol:function(){return"delayTime"}})(b.value),n=Oc(t);return function(u){return e(r(n(u)))}}};var OI=function(){return b.value}(),YC=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(L()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(OI)({tf:W(N(a)(a))(br("<|>")),txt:W(N(a)(a))(br(`run2_
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
  ]`)),constant:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.5)([JE(rE)(0)(M(O)(st())(q(k)(XC()(Ta)({d:5,o:.1,p:Gu(pf)(function(o){return E(function(){var c=tu(Do)(o)(3)===0;return c?1:0}())})(Qn(0)(1920))}))))])])}}))})}}};var RI=function(){return b.value}(),ZC=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(RI)({convolution:W(at(a)(a))(Lt(r)(t)(function(n){return Gt(lC)(_(ci)(function(u){return function(o){return{loop:u,verb:o}}})(wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(wt(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(u){return St(n)([UE(eE)(G(a)(a)(a))(u.verb)([he(qe)(u.loop)(st())])])}}))})}}};var LI=function(){return b.value}(),th=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
  <h2 id="delay">Delay</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/DelayNode">Delay</a>, as its name suggests, delays a signal. Using multiple delay nodes, you can create a decent echo effect.</p>

  <p>To create an even <i>better</i> echo effect, you can used fixed points, which is covered in the <a>Fix and fan</a> section of this documentation.</p>

  <pre><code>\\buf -> run2_
  [ fan1 (playBuf buf bangOn)
      \\b _ -> mix $ gain_ 0.2
        [ delay_ 0.03 [ b ]
        , delay_ 0.1 [ b ]
        , delay_ 0.3 [ b ]
        , delay_ 0.7 [ b ]
        ]
  ]</code></pre>

  @delay@
  </section>
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(b.value)(LI)({delay:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return St(n)([Ca(Nn(Na)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(.2)([oo(Xr)(G(a)(a)(a))(.03)([o]),oo(Xr)(G(a)(a)(a))(.1)([o]),oo(Xr)(G(a)(a)(a))(.3)([o]),oo(Xr)(G(a)(a)(a))(.7)([o])]))}})])}}))})}}};var qI=function(){return b.value}(),eh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
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
`}})()()(L()(Z)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))(qI)({gain:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.1)([he(qe)(u)(st())])])}}))})}}};var BI=function(){return b.value}(),rh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(BI)({highpass:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([xc(Ba)(G(a)(a)(a))(2e3)([he(qe)(u)(st())])])}}))})}}};var HI=function(){return b.value}(),nh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(HI)({highshelf:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([GE(PE(At(yt()(H(H(bt)(cE)()()()({reflectSymbol:function(){return"gain"}}))(lE)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:2e3,gain:.4})([he(qe)(u)(st())])])}}))})}}};var VI=function(){return b.value}(),ah=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="iir">IIR filter</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/IIRFilterNode">IIR filter</a>, or infinite impulse response filter, is the Swiss Army Knife of filters. You can carve out and boost parts of the spectrum with amazing precision. But it comes with a catch: you can't automate the parameters. The parameters are also tough to work with if you're new to IIR filters. In short, you're setting up coefficients for a filter of type:</p>

  <pre><code>x0s0 + x1s1 + x2s2 + ... + y0S0 + y1S1 + y2S2 + ...</code></pre>

  <p>Where <code>s1</code> is the unfiltered signal at time <code>t-1</code>, <code>S0</code> is the <i>filtered</i> signal at time <code>t-1</code>, etc. The xs and ys are often called <i>feedforward</i> and <i>feedback</i> coefficients respectively.</p>

  <p>Because the Web Audio API accepts between 3 and 20 parameters for feedforward and feedback coefficients, Wags enforces that through a <a href="https://github.com/bodil/purescript-sized-vectors">sized vector</a>.</p>

  <pre><code>\\{loop, verb} -> run2_
  [ iirFilter
      ( (0.00020298 +> 0.0004059599 +> 0.00020298 +> empty)
          /\\ (1.0126964558 +> -1.9991880801 +> 0.9873035442 +> empty)
      )
      [ loopBuf buf bangOn ]
  ]</code></pre>
  ~iirFilterEx~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(VI)({iirFilterEx:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([qE()()(G(a)(a)(a))(tE(a)(a))(G(a)(a)(a))(new et(ve(pe(Aa)()(wr)()(Gr))(20298e-8)(ve(pe(ka)()(hr)()(wr))(.0004059599)(ve(pe(ta)()(oa)()(hr))(20298e-8)(ga))),ve(pe(Aa)()(wr)()(Gr))(1.0126964558)(ve(pe(ka)()(hr)()(wr))(-1.9991880801)(ve(pe(ta)()(oa)()(hr))(.9873035442)(ga)))))([he(qe)(u)(st())])])}}))})}}};var jI=function(){return b.value}(),uh=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(b.value)(jI)({loopBuf:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(u){return St(n)([he(xf(At(yt()(H(H(H(H(bt)(Ac)()()()({reflectSymbol:function(){return"playbackRate"}}))(n_)()()()({reflectSymbol:function(){return"loopStart"}}))(r_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Tf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:u,playbackRate:.5,loopStart:.1,loopEnd:.6})(st()),he(xf(At(yt()(H(H(H(H(bt)(Ac)()()()({reflectSymbol:function(){return"playbackRate"}}))(n_)()()()({reflectSymbol:function(){return"loopStart"}}))(r_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Tf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:u,playbackRate:1,loopStart:.5,loopEnd:1.2})(st()),he(xf(At(yt()(H(H(bt)(Ac)()()()({reflectSymbol:function(){return"playbackRate"}}))(Tf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:u,playbackRate:1.7})(st())])}}))})}}};var QI=function(){return b.value}(),oh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))(QI)({lowpass:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Xp(ME)(G(a)(a)(a))(215)([he(qe)(u)(st())])])}}))})}}};var YI=function(){return b.value}(),ih=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(YI)({lowshelf:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([LE(wE(At(yt()(H(H(bt)(oE)()()()({reflectSymbol:function(){return"gain"}}))(iE)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:91,gain:.4})([he(qe)(u)(st())])])}}))})}}};var tR=function(){return b.value}(),fh=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(b.value)(tR)({microphone:W(at(a)(a))(Lt(r)(t)(function(n){return ns(!0)(!1)})(function(n){return function(u){return St(n)([function(){if(u.microphone instanceof U)return Ja(function(o){return re(_t)(G(a)(a)(a))(1)([i_(t_)(u.microphone.value0),oo(Xr)(G(a)(a)(a))(.1)([re(_t)(G(a)(a)(a))(.2)([o])])])});if(u.microphone instanceof z)return re(_t)(G(a)(a)(a))(.02)([zE(Hi)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[u.microphone.constructor.name])}()])}}))})}}};var rR=function(){return b.value}(),ch=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
<h2 id="notch">Notch filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">notch filter</a>, also known as a band-reject filter, attenuates a single frequency range of a source. When you crank up their Q value, the attenuation gets more intense. At the extreme, it sounds like part of the source got sucked into a vacuum, which is not un-interesting!</p>

  <pre><code>\\buf -> run2_
  [
    notch_ { frequency: 400.0, q: 1.0 }
    $ notch_ { frequency: 880.0, q: 5.0 }
    $ notch_ { frequency: 1200.0, q: 10.0 }
    $ notch_ { frequency: 2000.0, q: 20.0 }
    $ notch_ { frequency: 3000.0, q: 30.0 }
    $ loopBuf buf bangOn
  ]</code></pre>

  ~notch~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(rR)({notch:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Tc(gc(At(yt()(H(H(bt)(bc)()()()({reflectSymbol:function(){return"q"}}))(yc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:400,q:1})(Tc(gc(At(yt()(H(H(bt)(bc)()()()({reflectSymbol:function(){return"q"}}))(yc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:880,q:5})(Tc(gc(At(yt()(H(H(bt)(bc)()()()({reflectSymbol:function(){return"q"}}))(yc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:1200,q:10})(Tc(gc(At(yt()(H(H(bt)(bc)()()()({reflectSymbol:function(){return"q"}}))(yc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:2e3,q:20})(Tc(gc(At(yt()(H(H(bt)(bc)()()()({reflectSymbol:function(){return"q"}}))(yc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:3e3,q:30})(he(qe)(u)(st()))))))])}}))})}}};var aR=function(){return b.value}(),lh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="peaking">Peaking filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">peaking filter</a> is sort of like a notch/bandpass combo. It sounds different than bandpass or notch, and is often a better choice depending on what you're making. The Q works as normal, but the gain either boosts or attenuates the frequency in question if it is positive or negative.</p>

  <pre><code>\\buf -> run2_
  [
    peaking_ { frequency: 400.0, q: 1.0, gain: -20.0 }
    $ peaking_ { frequency: 880.0, q: 5.0, gain: 20.0 }
    $ peaking_ { frequency: 1200.0, q: 10.0, gain: -20.0 }
    $ peaking_ { frequency: 2000.0, q: 20.0, gain: 20.0 }
    $ peaking_ { frequency: 3000.0, q: 30.0, gain: -20.0 }
    $ loopBuf buf bangOn
  ]</code></pre>

  ~peaking~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(aR)({peaking:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Sc(kc(At(yt()(H(H(H(bt)(vc)()()()({reflectSymbol:function(){return"q"}}))(Dc)()()()({reflectSymbol:function(){return"gain"}}))(dc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:400,q:1,gain:-20})(Sc(kc(At(yt()(H(H(H(bt)(vc)()()()({reflectSymbol:function(){return"q"}}))(Dc)()()()({reflectSymbol:function(){return"gain"}}))(dc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:880,q:5,gain:20})(Sc(kc(At(yt()(H(H(H(bt)(vc)()()()({reflectSymbol:function(){return"q"}}))(Dc)()()()({reflectSymbol:function(){return"gain"}}))(dc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:1200,q:10,gain:-20})(Sc(kc(At(yt()(H(H(H(bt)(vc)()()()({reflectSymbol:function(){return"q"}}))(Dc)()()()({reflectSymbol:function(){return"gain"}}))(dc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:2e3,q:20,gain:20})(Sc(kc(At(yt()(H(H(H(bt)(vc)()()()({reflectSymbol:function(){return"q"}}))(Dc)()()()({reflectSymbol:function(){return"gain"}}))(dc)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(nr(a)(a)(a))({frequency:3e3,q:30,gain:-20})(he(qe)(u)(st()))))))])}}))})}}};var oR=function(){return b.value}(),_h=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
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
`}})()()(L()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(oR)({periodic:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.2)([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:140,spec:new et(ve(pe(Vn)()(Gr)()(Ia))(.1)(ve(pe(Aa)()(wr)()(Gr))(.2)(ve(pe(ka)()(hr)()(wr))(.3)(ve(pe(ta)()(oa)()(hr))(.4)(ga)))),ve(pe(Vn)()(Gr)()(Ia))(.4)(ve(pe(Aa)()(wr)()(Gr))(.3)(ve(pe(ka)()(hr)()(wr))(.2)(ve(pe(ta)()(oa)()(hr))(.1)(ga)))))})(st())])])}}))})}}};var fR=function(){return b.value}(),ph=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
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
`}})()()(L()(Z)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(fR)({playBuf:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(u){return St(n)([Nn(a_(At(yt()(H(H(H(bt)(aE)()()()({reflectSymbol:function(){return"duration"}}))(nE)()()()({reflectSymbol:function(){return"bufferOffset"}}))(e_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:u,duration:3,bufferOffset:4.2})(st())])}}))})}}};var Gd=function(){function t(){}return t.value=new t,t}();var sh={attr:function(t){return function(e){return v({key:"controls",value:R(e)})}}};var Bd=function(){function t(){}return t.value=new t,t}();var mh={attr:function(t){return function(e){return v({key:"src",value:R(e)})}}};var Ud=function(t){return function(e){return function(r){return V("audio")(e)(x(t)(r))}}};var sR=function(t){return function(e){return function(r){return function(n){return vD(t)(n)(i_(e)(r))}}}},mR=function(){return b.value}(),vh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(mR)({recorder:W(at(a)(a))(on(function(n){return function(u){var o=El(lu)(K(Y))(u),c=El(lu)(K(Y))(function(m){return m.left}(o)),f=function(m){return m.right}(c),l=Pc(O)(k)(r)(function(m){return m.right}(o)),p=function(m){return m.left}(c);return x(N(a)(a))(Je(Ee(a)(a))([sn(Ee(a)(a))(M(O)(q(k)(Q(_c)(qt.value)("cursor: pointer;")))(_(y)(function(m){return Q(pr)(cr.value)(Ze(E(function(){if(m.e instanceof Mo)return I(tt)(void 0);if(m.e instanceof Po)return J(j)(J(j)(J(j)(m.e.value0)(t(I(tt)(void 0))))(yu(tt)(rr)(m.rec)(function(){var s=Ll(tv);return function(d){return s(es(d))}}())))(n($t.create(pu.value)));if(m.e instanceof pu)return function(){m.cncl();var d=ro();n(new $t(Mo.value))();var i=fo(rt(Or)(_(ci)(function(h){return h.microphone})(ns(!0)(!1)))(function(h){return _r(Pr)(function(){var mt=Vr(I(tt)(I(tt)(void 0)))(function(fe){return function(){var ge=Jn(or)(),Nr=zi(ge)(),it=Ff(G(a)(a)(a))([sR(jv)(t_)(fe)(function(se){return function(){return n(new Ft(new $t(se)))(),Be(C)(xo(se)(d))(),hC("audio/ogg; codecs=opus")(function(dr){return n(Ft.create(Ft.create(dr)))})(se)()}})])(Pf),ne=kt(it)(function(se){return se(Nr)})();return function(){ne(),rt(Le)(Sk(d))(lr(tt)(rr)(function(){var dr=Ll(tv);return function(Lr){return dr(es(Lr))}}()))();var It=b_(or)(ge)();return dn(tt)(It!=="closed")(mn(or)(ge))()}}})(h)();return n(new $t(new Po(mt)))(),mt})}))();return t(function(){return n($t.create(pu.value))(),wo(_i(i))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[m.e.constructor.name])}())))})(En(k)(M(O)(q(k)(z.value))(_(y)(U.create)(f)))(_(y)(Qc)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(m){return m.value0})(r)))(_(y)(function(m){return function(s){return function(d){return{e:m,cncl:s,rec:d}}}})(l)))))))([rn(_(y)(function(m){if(m instanceof pu)return"Turn on";if(m instanceof Mo)return"Loading...";if(m instanceof Po)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[m.constructor.name])})(l))]),Je(Ee(a)(a))([Ud(N(a)(a))(M(O)(q(k)(Q(sh)(Gd.value)("true")))(M(O)(q(k)(Q(bv)(qt.value)("display:none;")))(M(O)(_(y)(function(m){return Q(mh)(Bd.value)(m)})(p))(_(y)(E(Q(bv)(qt.value)("display:block;")))(p)))))(ee)])]))}}))})}}};var DR=function(){return b.value}(),Dh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(DR)({periodic:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.2)([VE(Zg)(448)(st())])])}}))})}}};var bR=function(){return b.value}(),dh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(bR)({periodic:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.2)([$f(Hi)(448)(st())])])}}))})}}};var AR=function(){return b.value}(),bh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(AR)({periodic:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.2)([bD(Hp)(448)(st())])])}}))})}}};var gR=function(){return b.value}(),yh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(gR)({pan:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return St(n)([NE(Yg)(G(a)(a)(a))(1)([he(qe)(u)(st())])])}}))})}}};var CR=function(){return b.value}(),Ah=Nt({reflectType:function(){return`<ul>
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
`}})()()(Z)(CR)({});var SR=function(){return b.value}(),kh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(L()(Z)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(SR)({periodic:W(at(a)(a))(Lt(r)(t)(function(n){return I(fa)(void 0)})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(.2)([Yp(Up)(448)(st())])])}}))})}}};var xR=function(){return b.value}(),gh=function(t){return function(e){return function(r){return Nt({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(L()(L()(Z)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(xR)({code:W(N(a)(a))(br(`do
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
  wicked <- makeFloatArray (makeDistortionCurve 400.0)
  run2_
    [ waveShaper wicked [ loopBuf buf bangOn ] ]`)),waveShaper:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){var o=function(c){var f=Ni/180;return _(Oe)(function(l){var p=Ve(l)*2/Ve(44100)-1;return(3+c)*p*20*f/(Ni+c*qb(sa)(rf)(p))})(Qn(0)(44099))};return function(){var f=Cd(o(400))();return St(n)([RE($E)(G(a)(a)(a))(f)([he(qe)(u)(st())])])()}}}))})}}};var $R=function(){return b.value}(),Eh=function(t){return function(e){return function(r){return function(n){var u=J(j)(e(Mf.value))(Tn),o=Sa(t)(r);return Nt({reflectType:function(){return`<div>
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
</div>`}})()()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(Cn()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(L()(Z)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))($R)({drumroll:W(at(a)(a))(us("\u{1F941}")(n)(o)(function(c){return wt(c)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(c){return function(f){return St(c)([re(_t)(G(a)(a)(a))(1)([he(qe)(f)(st())])])}})),toc:W(N(a)(a))(Ah),allpass:W(N(a)(a))(IC(o)(e)(n)),analyser:W(N(a)(a))(VC(o)(e)(n)),bandpass:W(N(a)(a))(JC(o)(e)(n)),constant:W(N(a)(a))(YC(o)(e)(n)),compression:W(N(a)(a))(jC(o)(e)(n)),convolution:W(N(a)(a))(ZC(o)(e)(n)),delay:W(N(a)(a))(th(o)(e)(n)),gain:W(N(a)(a))(eh(o)(e)(n)),highpass:W(N(a)(a))(rh(o)(e)(n)),highshelf:W(N(a)(a))(nh(o)(e)(n)),iirFilter:W(N(a)(a))(ah(o)(e)(n)),loopBuf:W(N(a)(a))(uh(o)(e)(n)),lowshelf:W(N(a)(a))(ih(o)(e)(n)),lowpass:W(N(a)(a))(oh(o)(e)(n)),notch:W(N(a)(a))(ch(o)(e)(n)),playBuf:W(N(a)(a))(ph(o)(e)(n)),peaking:W(N(a)(a))(lh(o)(e)(n)),microphone:W(N(a)(a))(fh(o)(e)(n)),pan:W(N(a)(a))(yh(o)(e)(n)),periodicOsc:W(N(a)(a))(_h(o)(e)(n)),recorder:W(N(a)(a))(vh(o)(e)(n)),sawtoothOsc:W(N(a)(a))(Dh(o)(e)(n)),sinOsc:W(N(a)(a))(dh(o)(e)(n)),squareOsc:W(N(a)(a))(bh(o)(e)(n)),triangleOsc:W(N(a)(a))(kh(o)(e)(n)),waveShaper:W(N(a)(a))(gh(o)(e)(n)),next:ja(O)(k)(n)(u)})}}}};var Hd=function(){function t(){}return t.value=new t,t}(),Ch={attr:function(t){return function(e){return v({key:"checked",value:R(e)})}}};var lo=function(){function t(){}return t.value=new t,t}();var Io={attr:function(t){return function(e){return v({key:"type",value:R(e)})}}};var _o=function(t){return function(e){return function(r){return V("input")(e)(x(t)(r))}}};var OR=function(t){return t},ys=function(t){return function(e){return function(r){return no(t)(M(t.Plus0().Alt0())(q(t)(e))(r))}}};var g_=function(t){return function(e){return t(e)}},ji=function(t){return{map:function(e){return function(r){return function(n){return r(_(t)(function(u){return function(o){return u(e(o))}})(n))}}}}},mi=function(t){return function(e){return function(r){return function(n){return g_(_(ji(t.Filterable1().Functor1()))(e)(r))(_(t.Filterable1().Functor1())(Bf)(n))}}}};var Lc=function(t){return mi(t)(E)};var Xa=OR;var hh=function(t){return function(e){return Xa(function(r){return ri(k)(M(O)(q(k)(g_(t)(r)))(_(y)(function(n){return g_(n)(r)})(e)))})}},zd=function(t){return{apply:function(e){return function(r){return function(n){return r(e(_(t)(po(qo))(n)))}}},Functor0:function(){return ji(t)}}};var As="_____$__$_$$_vbus";function Vd(t){return t[As]=As,t}function Jd(t){return()=>{for(let e in t)delete t[e]}}function jd(t){return()=>{let e=(o,c,f,l)=>{let p=Object.keys(l);for(var m=0;m<p.length;m++)if(l[p[m]]instanceof Object&&l[p[m]][As]===As){let s={},d={};e(o,s,d,l[p[m]]),c[p[m]]=s,f[p[m]]=d}else{let s=`${Math.random()}`;o[s]={},c[p[m]]=d=>()=>{let i=Object.keys(o[s]);for(var h=0;h<i.length;h++)o[s][i[h]](d)()},f[p[m]]=d=>()=>{let i=`${Math.random()}`;return o[s][i]=d,()=>{delete o[s][i]}}}},r={},n={},u={};return e(r,n,u,t),{p:n,e:u,s:r}}}var Wn={vb:function(t){return function(e){return function(r){return{}}}}},ks=function(t){return t.vb},$u=function(){return function(t){return function(e){return function(r){var n=ks(t)(b.value)(b.value)(b.value);return Pt(function(u){return function(){var c=jd(n)();return u(r(c.p)(c.e))(),Jd(c.s)}})}}}},wu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(u){return function(o){return Il(t)()()(b.value)(Vd(ks(e)(b.value)(b.value)(b.value)))(ks(r)(b.value)(b.value)(b.value))}}}}}}}}}}}}}},ir=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(u){return Il(t)()()(b.value)(void 0)(ks(e)(b.value)(b.value)(b.value))}}}}}}}}}}}};function qc(t){return function(e){return function(){return qc(e,t)}}}function Lf(t){return function(){Lf(t)}}var LR={eq:function(t){return function(e){return t===e}}},gs={compare:function(t){return function(e){return zt(Yr)(t)(e)}},Eq0:function(){return LR}};var Gc=function(t){return function(e){return Pt(function(r){return kt(e)(function(n){return function(){var o=d_(t)();return r({acTime:o,value:n})()}})})}};var Th=function(t){return function(e){return function(r){var n=function(u){return function(o){return function(c){return function(f){return function(l){return function(p){return function(){var s=Ce(c)();return dn(tt)(s)(function(){var i=d_(t)(),h=qc(MA(du(sa)(o-i-.04)(.01)*1e3))(function(){var mt=Ce(c)();return dn(tt)(mt)(function(){return ze(o)(l)(),u(o)(),n(u)(o+p)(c)(f)(l)(p)()})()})();return ze(new U(h))(f)()})()}}}}}}};return Pt(function(u){return function(){var c=ye(!0)(),f=ye(z.value)(),l=d_(t)(),p=ye(l+e)();n(u)(e)(c)(f)(p)(e)();var m=kt(r)(function(s){return function(){rt(Le)(Ce(f))(lr(tt)(rr)(Lf))();var i=Ce(p)();return n(u)(i+s)(c)(f)(p)(s)()}})();return J(j)(J(j)(m)(ze(!1)(c)))(rt(Le)(Ce(f))(lr(tt)(rr)(Lf)))}})}}};var xa=function(t){return function(e){return function(r){return function(n){return function(u){var o=r===t||n===e;if(o)return e;var c=(n-e)/(r-t),f=e-c*t;return c*u+f}}}}};var WR=function(){return b.value}(),xh=function(t){return function(e){return function(r){return function(n){return Nt({reflectType:function(){return`<section>
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

</section>`}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(WR)({txt:W(N(a)(a))(br(`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Tuple.Nested ((/\\))
import Data.Vec ((+>))
import Data.Vec as V
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, text)
import Deku.DOM as D
import Deku.Toplevel (runInBody1)
import Effect (Effect)
import FRP.Behavior (sampleBy, sample_, step)
import FRP.Event (memoize)
import FRP.Event.Animate (animationFrameEvent)
import FRP.Event.Class (bang, fold, mapAccum, sampleOn)
import FRP.Event.VBus (V, vbus)
import Math (pi, sin)
import Type.Proxy (Proxy(..))
import WAGS.Clock (withACTime)
import WAGS.Control (gain, periodicOsc)
import WAGS.Interpret (close, constant0Hack, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
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
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $> do
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
                                      ( oneOf
                                          [ bangOn
                                          , evs ev2 ev3 <#> \\{ f, a, t } -> P.frequency
                                              $ AudioNumeric
                                                { n: 325.6 +
                                                    (calcSlope 1.0 3.0 4.0 15.5 a * sin (pi * f))
                                                , o: t
                                                , t: _linear
                                                }
                                          ]
                                      )
                                  ]
                              ]
                      )
                    push.startStop.stop (r *> c0h *> close ctx)
                , stopE <#> (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        , D.div
            ( oneOfMap (map (attr D.Style))
                [ stopE $> "display:block;"
                , startE $> "display:none;"
                ]
            )
            ( map
                ( \\e -> D.input
                    ( oneOf
                        [ bang (D.Xtype := "checkbox")
                        , bang (D.OnClick := cb (const (e unit)))
                        , startE $> (D.Checked := "false")
                        ]
                    )
                    blank
                )
                ([ _.cbx0, _.cbx1, _.cbx2, _.cbx3 ] <@> push.cbx)
            )
        ]
  )`)),empl:W(at(a)(a))($u()(wu({reflectSymbol:function(){return"cbx"}})()()()(ir({reflectSymbol:function(){return"cbx0"}})()()(ir({reflectSymbol:function(){return"cbx1"}})()()(ir({reflectSymbol:function(){return"cbx2"}})()()(ir({reflectSymbol:function(){return"cbx3"}})()()(Wn)()()()())()()()())()()()())()()()())(wu({reflectSymbol:function(){return"startStop"}})()()()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(b.value)(function(u){return function(o){var c=M(O)(q(k)(void 0))(o.startStop.start),f=function(d){return ys(k)(!1)(Cu(k)(E(nu(wa)))(d)(!1))},l=f(o.cbx.cbx3),p=f(o.cbx.cbx2),m=f(o.cbx.cbx1),s=f(o.cbx.cbx0);return x(N(a)(a))(Je(Ee(a)(a))([sn(Ee(a)(a))(Ir(Jt)(g)(_(y)(function(){var d=Q(pr)(cr.value);return function(i){return d(Ze(E(i)))}}()))([er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(d){return d.value0})(n)))(X(y)(c)(K(Y))))(function(d){return function(){d();var h=Jn(or)(),ct=Fu(or)(h)(),mt=function(ge){return function(Nr){return function(it){return Jl(k)(function(ne){return function(se){var It=se.value1+(ne.value1-se.value0)*function(){return ne.value0?ge:1}();return new et(new et(ne.value1,It),It)}})(mi(k)(et.create)(Nr)(it))(new et(0,0))}}},fe=Ic(h)(Gi(_(y)(function(){var ge=Ue(_a)(.04);return function(Nr){return ge(function(it){return it.acTime}(Nr))}}())(Gc(h)(Ji)))(function(ge){var Nr=function(dr){return function(Lr){return no(k)(ge)(_(y)(Qc)(no(k)(Lr)(_(y)(function(Wr){return function(Ro){return function(Qa){return{f:Wr,a:Ro,t:Qa}}}})(dr))))}},it=_(y)(function(dr){return dr?4:1})(Lc(k)(l)(ge)),ne=mt(4)(p)(ge),se=_(y)(function(dr){return dr?4:1})(Lc(k)(m)(ge)),It=mt(8)(s)(ge);return[Br(_t)(G(a)(a)(a))(0)(er(y)(Nr(It)(se))(function(dr){return Dn()(xn)({n:xa(1)(.01)(4)(.15)(dr.a)*up(Ni*dr.f)+.15,o:dr.t,t:Vi})}))([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:325.6,spec:new et(ve(pe(Vn)()(Gr)()(Ia))(.3)(ve(pe(Aa)()(wr)()(Gr))(-.1)(ve(pe(ka)()(hr)()(wr))(.7)(ve(pe(ta)()(oa)()(hr))(-.4)(ga)))),ve(pe(Vn)()(Gr)()(Ia))(.6)(ve(pe(Aa)()(wr)()(Gr))(.3)(ve(pe(ka)()(hr)()(wr))(.2)(ve(pe(ta)()(oa)()(hr))(0)(ga)))))})(Jr(Jt)(g)([st(),er(y)(Nr(ne)(it))(function(dr){return co()(xn)({n:325.6+xa(1)(3)(4)(15.5)(dr.a)*up(Ni*dr.f),o:dr.t,t:Vi})})]))])]}))(),jt=J(j)(J(j)(fe)(ct))(mn(or)(h));return t(J(j)(jt)(u.startStop.start(void 0)))(),u.startStop.stop(jt)()}}),er(y)(o.startStop.stop)(function(d){return J(j)(d)(J(j)(t(I(tt)(void 0)))(u.startStop.start(void 0)))})]))([rn(Jr(Jt)(g)([X(y)(c)("Turn on"),X(y)(o.startStop.stop)("Turn off")]))]),Ae(Ee(a)(a))(Ir(Jt)(g)(_(y)(Q(lt)(qt.value)))([X(y)(o.startStop.stop)("display:block;"),X(y)(c)("display:none;")]))(_(Oe)(function(d){return _o(N(a)(a))(Jr(Jt)(g)([q(k)(Q(Io)(lo.value)("checkbox")),q(k)(Q(pr)(cr.value)(Ze(E(d(void 0))))),X(y)(c)(Q(Ch)(Hd.value)("false"))]))(ee)})(Is(Oe)([function(d){return d.cbx0},function(d){return d.cbx1},function(d){return d.cbx2},function(d){return d.cbx3}])(u.cbx)))]))}}))})}}}};var Xd={recip:function(t){return 1/t},Ring0:function(){return rf}};var Qd=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function Bc(t){return function(){return function(e){return t(e)()}}}function Uc(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function Hc(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function Kd(t){return t.clientX}function Yd(t){return t.clientY}function E_(t){return t.button}var C_=xt("MouseEvent");var Fh=function(t){return function(e){return Pt(function(r){return kt(e)(function(n){return function(){var o=Ce(t.buttons)();return r({value:n,buttons:o})()}})})}};var $h=function(){var e=ye(z.value)(),r=ye(Gm)(),n=_(C)(qd)(si)(),u=Bc(function(l){return lr(tt)(rr)(function(p){return ze(new U({x:Kd(p),y:Yd(p)}))(e)})(C_(l))})(),o=Bc(function(l){return lr(tt)(rr)(function(p){return Ti(RA(Yr)(E_(p)))(r)})(C_(l))})(),c=Bc(function(l){return lr(tt)(rr)(function(p){return Ti(ip(Yr)(E_(p)))(r)})(C_(l))})();Uc(tn()("mousemove"))(u)(!1)(n)(),Uc(tn()("mousedown"))(o)(!1)(n)(),Uc(tn()("mouseup"))(c)(!1)(n)();var f=function(){return Hc(tn()("mousemove"))(u)(!1)(n)(),Hc(tn()("mousedown"))(o)(!1)(n)(),Hc(tn()("mouseup"))(c)(!1)(n)()};return{position:e,buttons:r,dispose:f}},wh=Pt(function(t){return function(){var r=_(C)(qd)(si)(),n=Bc(function(u){return lr(tt)(rr)(function(o){return t(E_(o))})(C_(u))})();return Uc(tn()("mousedown"))(n)(!1)(r)(),Hc(tn()("mousedown"))(n)(!1)(r)}});var Ph=function(t){return Xa(function(e){return _(y)(function(r){return r.value(r.buttons)})(Fh(t)(e))})};var eb=function(t){return t};function hs(){return Date.now()}var aS=function(t){return Pt(function(e){return kt(t)(function(r){return function(){var u=hs();return e({time:u,value:r})()}})})};var uS=function(t){return function(e){return Pt(function(r){return function(){var u=ye(be(LA(gs)))(),o=kt(e)(function(c){return function(){var l=ye(z.value)(),p=qc(t)(function(){r(c)();var s=Ce(l)();return Vr(I(tt)(void 0))(function(d){return Kn(ip(gs)(d))(u)})(s)()})();return ze(new U(p))(l)(),Kn(Ct(qm(gs))(OA(p)))(u)()}})();return function(){var f=Ce(u)();return yu(tt)(NA)(f)(Lf)(),o()}}})}};var AN=Xa(function(t){return _(y)(function(e){return e.value(e.time)})(aS(t))}),nb=_(ji(y))(function(){var t=c_(fC);return function(e){return t(eb(e))}}())(AN);var gN=function(t){var e=function(o){return function(c){return function(f){return function(l){return function(p){return function(m){return function(s){var d=Ue(c.DivisionRing1().Ring0().Semiring0())(pa(c.DivisionRing1().Ring0().Semiring0()))(pa(c.DivisionRing1().Ring0().Semiring0())),i=function(h){return function(ct){if(h.last instanceof z)return ct;if(h.last instanceof U)return Ue(f)(ct)(l(function(mt){return Lu(c.EuclideanRing0())(Fn(c.DivisionRing1().Ring0().Semiring0())(mt(Ue(f)(h.last.value0.value1)(h.now.value1)))(vu(c.DivisionRing1().Ring0())(h.now.value0)(h.last.value0.value0)))(d)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[h.constructor.name,ct.constructor.name])}};return Xa(function(h){var ct=g_(s)(X(o.Filterable1().Functor1())(h)(K(Y))),mt=Op(o)(mi(o)(et.create)(m)(ct)),fe=Cu(o)(i)(mt)(p);return no(o)(fe)(h)})}}}}}}},r=function(o){return function(c){return e(o)(c)(c.DivisionRing1().Ring0().Semiring0())(function(f){return f(K(Y))})}},n=function(o){return function(c){return Xa(function(f){return jl(k)(function(l){var p=c(ys(k)(o)(l));return{input:Lc(k)(p)(f),output:no(k)(l)(f)}})})}},u=function(o){return function(c){return function(f){if(IA(o))return-8*(c-1)-f*2;if(tr)return 2*(4-c);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[o.constructor.name,c.constructor.name,f.constructor.name])}}};return n(2)(function(o){return r(k)(Qd(ol)(Xd))(2)(_(ji(y))(bn())(nb))(function(){var c=n(10)(function(f){return r(k)(Qd(ol)(Xd))(10)(_(ji(y))(bn())(nb))(Gt(zd(y))(Gt(zd(y))(_(ji(y))(u)(Ph(t)))(o))(f))});return hh(c)(X(y)(wh)(c))}())})},EN=function(){return b.value}(),iS=function(t){return function(e){return function(r){return function(n){return Nt({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(EN)({txt:W(N(a)(a))(br(`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
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
import WAGS.Parameter (AudioNumeric(..), _linear, bangOn)
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
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $>
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
                , event.stop <#> (_ *> push.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , stopE $> "Turn off"
                ]
            ]
        ]
  )`)),empl:W(at(a)(a))($u()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(b.value)(function(u){return function(o){var c=M(O)(q(k)(void 0))(o.start);return x(N(a)(a))(Je(Ee(a)(a))([sn(Ee(a)(a))(Ir(Jt)(g)(_(y)(function(){var f=Q(pr)(cr.value);return function(l){return f(Ze(E(l)))}}()))([er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(f){return f.value0})(n)))(X(y)(c)(K(Y))))(function(f){return function(){f();var p=Jn(or)(),m=Fu(or)(p)(),s=$h(),d=Ul(0)(1e4)(),i=function(it){return{o:it.value0+.04,n:it.value1,t:Vi}},h=_(eo)(function(it){return it-.5})(fv(vk)),ct=rt(gf)(h)(function(it){return rt(gf)(h)(function(ne){return rt(gf)(h)(function(se){return rt(gf)(h)(function(It){return I(Hl)(ve(pe(Vn)()(Gr)()(Ia))(it)(ve(pe(Aa)()(wr)()(Gr))(ne)(ve(pe(ka)()(hr)()(wr))(se)(ve(pe(ta)()(oa)()(hr))(It)(ga)))))})})})}),mt=Gt(Ef)(_(eo)(et.create)(ct))(ct),fe=Gt(Ef)(Gt(Ef)(Gt(Ef)(_(eo)(function(it){return function(ne){return function(se){return function(It){return{s0:it,s1:ne,s2:se,s3:It}}}}})(mt))(mt))(mt))(mt),jt=Cp(fe)({newSeed:Ep(d),size:5}),ge=Ic(p)(Gi(_(y)(function(it){return new et(it.acTime,it.value)})(Gc(p)(Lc(k)(gN(s))(Ji))))(function(it){return[Br(_t)(G(a)(a)(a))(0)(_(y)(function(){var ne=Dn()(xn),se=Ln(vn)(function(It){return du(sa)(-.4)(.5*(It-1))});return function(It){return ne(i(se(It)))}}())(it))([Xp(iD(At(yt()(H(H(bt)(fE)()()()({reflectSymbol:function(){return"q"}}))(Xv)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:90.4,q:20})([HE(Hp)(90.4)])]),Br(_t)(G(a)(a)(a))(0)(_(y)(function(){var ne=Dn()(xn),se=Ln(vn)(function(It){return du(sa)(-.2)(.4*(It-3))});return function(It){return ne(i(se(It)))}}())(it))([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:90.4*4,q:20})([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*3.02,spec:jt.s0})(M(O)(st())(_(y)(function(){var ne=co()(xn),se=Ln(vn)(function(It){return 90.4*3.02+14*(It-1)});return function(It){return ne(i(se(It)))}}())(it)))])]),Br(_t)(G(a)(a)(a))(0)(_(y)(function(){var ne=Dn()(xn),se=Ln(vn)(function(It){return du(sa)(-.1)(.2*(It-6))});return function(It){return ne(i(se(It)))}}())(it))([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:90.4*6,q:20})([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*5.07,spec:jt.s1})(M(O)(st())(_(y)(function(){var ne=co()(xn),se=Ln(vn)(function(It){return 90.4*5.07+18*(It-1)});return function(It){return ne(i(se(It)))}}())(it)))])]),Br(_t)(G(a)(a)(a))(0)(_(y)(function(){var ne=Dn()(xn),se=Ln(vn)(function(It){return du(sa)(0)(.2*(It-3))});return function(It){return ne(i(se(It)))}}())(it))([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:90.4*8,q:20})([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*7.13,spec:jt.s2})(M(O)(st())(_(y)(function(){var ne=co()(xn),se=Ln(vn)(function(It){return 90.4*7.13+32*(It-1)});return function(It){return ne(i(se(It)))}}())(it)))])]),Br(_t)(G(a)(a)(a))(0)(_(y)(function(){var ne=Dn()(xn),se=Ln(vn)(function(It){return du(sa)(0)(.1*(It-7))});return function(It){return ne(i(se(It)))}}())(it))([fi(ii(At(yt()(H(H(bt)(oi(ai(Vn)))()()()({reflectSymbol:function(){return"spec"}}))(ui)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))({frequency:90.4*9.14,spec:jt.s3})(M(O)(st())(_(y)(function(){var ne=co()(xn),se=Ln(vn)(function(It){return 90.4*9.14+31*(It-1)});return function(It){return ne(i(se(It)))}}())(it)))])]}))(),Nr=J(j)(J(j)(ge)(m))(mn(or)(p));return t(J(j)(Nr)(u.start(void 0)))(),u.stop(Nr)()}}),er(y)(o.stop)(function(f){return J(j)(f)(J(j)(t(I(tt)(void 0)))(u.start(void 0)))})]))([rn(Jr(Jt)(g)([X(y)(c)("Turn on"),X(y)(o.stop)("Turn off")]))])]))}}))})}}}};var hN=function(){return b.value}(),fS=function(t){return function(e){return function(r){return function(n){var u=Sa(t)(r);return Nt({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(L()(L()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(hN)({next:ja(O)(k)(n)(J(j)(e(m_.value))(Tn)),fold:W(N(a)(a))(xh(u)(e)(r)(n)),fix:W(N(a)(a))(iS(u)(e)(r)(n))})}}}};var TN=function(){function t(){}return t.value=new t,t}(),cS=function(){function t(){}return t.value=new t,t}(),ab=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),xN=`module Main where

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
import WAGS.Parameter (AudioEnvelope(..), AudioOnOff(..), _on, _off)
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
`;var FN=function(){return b.value}(),$N=function(t){return function(e){return function(r){return q(t)(Nf(e)(If)({x:Nd,o:r}))}}},wN=function(t){return function(e){return function(r){return q(t)(Nf(e)(If)({x:OC,o:r}))}}},MN=uo(Ra)(Ve)(function(t){var e=function(u){return M(O)($N(k)()(u+.27*(t*Ri(1.005)(t))))(wN(k)()(u+3+.3*(t*Ri(1.005)(t))))},r=function(u){return q(k)(Dn()(Ta)({p:[0,.4,.1,.05,.01,0],o:u+.3*(t*Ri(1.005)(t)),d:.8}))},n=function(u){return function(o){return Br(_t)(G(a)(a)(a))(0)(r(u))([$f(Hi)(200+t*o)(e(u))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),lS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(b.value)(FN)({txt:W(N(a)(a))(br(xN)),ex0:W(at(a)(a))(on(function(n){return uo(Ra)(function(u){return M(O)(q(k)(TN.value))(u)})(function(u){return x(N(a)(a))(Je(Ee(a)(a))([sn(Ee(a)(a))(er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(o){return o.value0})(r)))(_(y)(et.create)(u)))(function(o){return Q(pr)(cr.value)(Ze(E(function(){return o.value0 instanceof ab?J(j)(J(j)(o.value0.value0)(n(cS.value)))(t(I(tt)(void 0))):function(){o.value1();var f=ls([re(_t)(G(a)(a)(a))(1)(Ya(Yc)(_(Oe)(MN)(Qn(0)(100))))])();return t(J(j)(f)(n(cS.value)))(),n(new ab(f))()}}())))}))([rn(er(y)(u)(function(o){return o instanceof ab?"Turn off":"Turn on"}))])]))})}))})}}};var vi=function(){function t(){}return t.value=new t,t}();var Qi={attr:function(t){return function(e){return v({key:"max",value:R(e)})}}};var Di=function(){function t(){}return t.value=new t,t}();var Ki={attr:function(t){return function(e){return v({key:"min",value:R(e)})}}};var di=function(){function t(){}return t.value=new t,t}();var Yi={attr:function(t){return function(e){return v({key:"input",value:ut(e)})}}};var bi=function(){function t(){}return t.value=new t,t}(),Zi={attr:function(t){return function(e){return v({key:"step",value:R(e)})}}};var yi=function(){function t(){}return t.value=new t,t}();var tf={attr:function(t){return function(e){return v({key:"value",value:R(e)})}}};function _S(t){return t.target}var zc=function(t){return jr(_S(t))};var RN=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, plant, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create)
import FRP.Event.Class (bang, biSampleOn, keepLatest)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (loopBuf)
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Math (calcSlope)
import WAGS.Parameter (bangOn)
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
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ keepLatest $ vbus (Proxy :: _ UIEvents) \\push event -> do
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
              $ oneOf
                [ bangOn
                , map
                    (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate)
                    sl0
                , map
                    (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart)
                    sl1
                , map
                    (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd)
                    (biSampleOn sl2 (add <$> (bang 0.0 <|> sl1)))
                ]
          ]
      plant $ D.div_
        $
          map
            ( \\{ l, f } -> D.div_
                [ text_ l
                , D.input
                    ( oneOfMap bang
                        [ D.Xtype := "range"
                        , D.Min := "0"
                        , D.Max := "100"
                        , D.Step := "1"
                        , D.Value := "50"
                        , D.OnInput := cb
                            ( traverse_
                                (valueAsNumber >=> f)
                                <<< (=<<) fromEventTarget
                                <<< target
                            )
                        ]
                    )
                    blank
                ]
            )
            [ { l: "Playback rate", f: push.slider.s0 }
            , { l: "Loop start", f: push.slider.s1 }
            , { l: "Loop end", f: push.slider.s2 }
            ] <>
            [ D.button
                ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                    [ start $> (music >>= push.startStop.stop)
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
`,NN=function(){return b.value}(),LN="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",pS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(L()(L()(L()(Z)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(b.value)(NN)({wagtxt:W(N(a)(a))(br(`run2_
  $ loopBuf
      { buffer: buffer
      , playbackRate: 2.6
      , loopStart: 0.6
      , loopEnd: 1.1
      }
  $ oneOf
      [ bangOn
      , (calcSlope 0.0 0.2 100.0 5.0 >>> playbackRate) <$> sl0
      , (calcSlope 0.0 0.0 100.0 1.2 >>> loopStart) <$> sl1
      , (calcSlope 0.0 0.05 100.0 1.0 >>> loopEnd) <$> biSampleOn sl2
          (add <$> (bang 0.0 <|> sl1))
      ]`)),txt:W(N(a)(a))(br(RN)),ex1:W(at(a)(a))($u()(wu({reflectSymbol:function(){return"slider"}})()()()(ir({reflectSymbol:function(){return"s0"}})()()(ir({reflectSymbol:function(){return"s1"}})()()(ir({reflectSymbol:function(){return"s2"}})()()(Wn)()()()())()()()())()()()())(wu({reflectSymbol:function(){return"startStop"}})()()()(ir({reflectSymbol:function(){return"loading"}})()()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(b.value)(function(n){return function(u){var o=M(O)(u.startStop.start)(q(k)(void 0)),c=function(f){return he(xf(At(yt()(H(H(H(H(bt)(Ac)()()()({reflectSymbol:function(){return"playbackRate"}}))(n_)()()()({reflectSymbol:function(){return"loopStart"}}))(r_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Tf)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:f,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(Jr(Jt)(g)([st(),_(y)(function(){var l=bs()(cs),p=xa(0)(.2)(100)(5);return function(m){return l(p(m))}}())(u.slider.s0),_(y)(function(){var l=QC(),p=xa(0)(0)(100)(1.2);return function(m){return l(p(m))}}())(u.slider.s1),_(y)(function(){var l=KC(),p=xa(0)(.05)(100)(1);return function(m){return l(p(m))}}())(En(k)(u.slider.s2)(_(y)(Ue(_a))(M(O)(q(k)(0))(u.slider.s1))))]))};return x(N(a)(a))(Je(Ee(a)(a))(Ct(Ka)(_(Oe)(function(f){return Je(Ee(a)(a))([br(f.l),_o(N(a)(a))(Ir(Jt)(g)(q(k))([Q(Io)(lo.value)("range"),Q(Ki)(Di.value)("0"),Q(Qi)(vi.value)("100"),Q(Zi)(bi.value)("1"),Q(tf)(yi.value)("50"),Q(Yi)(di.value)(Ze(function(){var l=lr(tt)(rr)(Hf(Le)(Rf)(f.f)),p=Gn(va)(Rc);return function(m){return l(p(zc(m)))}}()))]))(ee)])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([sn(Ee(a)(a))(Ir(Jt)(g)(_(y)(function(){var f=Q(pr)(cr.value);return function(l){return f(Ze(E(l)))}}()))([X(y)(u.startStop.loading)(I(tt)(void 0)),er(y)(u.startStop.stop)(function(f){return J(j)(f)(J(j)(t(I(tt)(void 0)))(n.startStop.start(void 0)))}),er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(f){return f.value0})(r)))(X(y)(o)(K(Y))))(function(f){return function(){f(),n.startStop.loading(void 0)();var p=fo(rt(Or)(Jn(Pr))(function(m){return rt(Or)(Fu(Pr)(m))(function(s){return rt(Or)(wt(m)(LN))(function(d){return _r(Pr)(function(){var h=St(m)([c(d)])(),ct=J(j)(J(j)(h)(s))(mn(or)(m));return n.startStop.stop(ct)(),ct})})})}))();return t(function(){return n.startStop.start(void 0)(),wo(_i(p))()})(),void 0}})]))([rn(Jr(Jt)(g)([_(y)(E("Turn off"))(u.startStop.stop),_(y)(E("Turn on"))(o)]))])])))}}))})}}};var qN=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap, traverse_)
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, text, text_)
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
import WAGS.Core (Node, mix)
import WAGS.Interpret (close, context)
import WAGS.Math (calcSlope)
import WAGS.Parameter (AudioEnvelope(..), bangOn)
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
        music :: forall lock. _ -> Event (Array (Node _ lock _))
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
              mix $ gain_ 2.0
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
                ( oneOfMap bang
                    [ D.Xtype := "range"
                    , D.Min := "0"
                    , D.Max := "100"
                    , D.Step := "1"
                    , D.Value := "50"
                    , D.OnInput := cb
                        ( traverse_
                            (valueAsNumber >=> push.slider)
                            <<< (=<<) fromEventTarget
                            <<< target
                        )
                    ]
                )
                blank
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
  )`,GN=Xa(function(t){return Pt(function(e){return kt(t)(function(r){return function(){var u=Ga();return e(r(u))()}})})}),BN=function(){return b.value}(),UN=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(tr)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},sS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(b.value)(BN)({txt:W(N(a)(a))(br(qN)),ex2:W(at(a)(a))($u()(ir({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())(Wn)()()()())()()()())(b.value)(function(n){return function(u){var o=M(O)(u.startStop.start)(q(k)(void 0)),c=function(f){return Gi(f)(function(l){var p=_(y)(function(){var ct=Ue(_a)(.01);return function(mt){return ct(Zr(mt))}}())(l),m=_(y)(Pa)(l),s=M(O)(st())(_(y)(function(){var ct=co()(cs);return function(mt){return ct(UN(mt))}}())(m)),d=_(y)(function(ct){return is(function(mt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:mt}}(ct))})(p),i=_(y)(function(ct){return is(function(mt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:mt}}(ct))})(p),h=_(y)(function(ct){return is(function(mt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:mt}}(ct))})(p);return[Ca(Yp(Up)(0)(s))(function(ct){return function(mt){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(2)([Br(_t)(G(a)(a)(a))(0)(_(y)(Dn()(Ta))(h))([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:1e3,q:20})([ct])]),Br(_t)(G(a)(a)(a))(0)(_(y)(Dn()(Ta))(i))([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:2e3,q:20})([ct])]),Br(_t)(G(a)(a)(a))(0)(_(y)(Dn()(Ta))(d))([xc(fD(At(yt()(H(H(bt)(_E)()()()({reflectSymbol:function(){return"q"}}))(Qv)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:4e3,q:20})([ct])])]))}})]})};return x(N(a)(a))(Je(Ee(a)(a))([Je(Ee(a)(a))([br("tempo"),_o(N(a)(a))(Ir(Jt)(g)(q(k))([Q(Io)(lo.value)("range"),Q(Ki)(Di.value)("0"),Q(Qi)(vi.value)("100"),Q(Zi)(bi.value)("1"),Q(tf)(yi.value)("50"),Q(Yi)(di.value)(Ze(function(){var f=lr(tt)(rr)(Hf(Le)(Rf)(n.slider)),l=Gn(va)(Rc);return function(p){return f(l(zc(p)))}}()))]))(ee)]),sn(Ee(a)(a))(Ir(Jt)(g)(_(y)(function(){var f=Q(pr)(cr.value);return function(l){return f(Ze(E(l)))}}()))([er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(f){return f.value0})(r)))(X(y)(o)(K(Y))))(function(f){return function(){f();var p=Jn(or)(),m=mi(k)(et.create)(GN)(Th(p)(.91)(_(y)(xa(0)(.42)(100)(1.4))(u.slider))),s=Ic(p)(c(m))(),d=J(j)(s)(mn(or)(p));return t(J(j)(d)(n.startStop.start(void 0)))(),n.startStop.stop(J(j)(d)(mn(or)(p)))()}}),er(y)(u.startStop.stop)(function(f){return J(j)(f)(J(j)(t(I(tt)(void 0)))(n.startStop.start(void 0)))})]))([rn(Jr(Jt)(g)([X(y)(o)("Turn on"),X(y)(u.startStop.stop)("Turn off")]))])]))}}))})}}};var zN=function(){return b.value}(),mS=function(){return De({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(Z)(b.value)(zN)({})}();var JN=function(){return b.value}(),vS=function(){return De({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(Z)(b.value)(JN)({})}();var XN=function(){return b.value}(),DS=function(){return De({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(Z)(b.value)(XN)({})}();var KN=function(){return b.value}(),dS=function(t){return function(e){return function(r){return function(n){var u=function(c){return ja(O)(k)(n)(J(j)(e(c))(Tn))},o=Sa(t)(r);return De({reflectType:function(){return`<div>
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
  <p>In this section, saw how to build rich audio applications using the <code>Event</code> and <code>Behavior</code> types. We also covered the three most common patterns you'll see when working with events: events that need to happen <i>now</i>, events that come from user interaction, and timed events. In the next section, we'll look at how to make events <a @next@ style="cursor:pointer;">stateful</a>.</p>
</div>`}})({reflectType:function(){return"@"}})()()(L()(L()(L()(L()(L()(Cn()(L()(Z)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(b.value)(KN)({next:u(s_.value),primer:W(N(a)(a))(DS),inWags:W(N(a)(a))(vS),flavors:W(N(a)(a))(mS),ex0:W(N(a)(a))(lS(o)(e)(n)),ex1:W(N(a)(a))(pS(o)(e)(n)),ex2:W(N(a)(a))(sS(o)(e)(n))})}}}};var ZN=function(){return b.value}(),bS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(ZN)({ai0:W(at(a)(a))(Lt(r)(t)(function(n){return io(Sn)(Gt(li)(Gt(li)(Gt(li)(_(wc)(function(u){return function(o){return function(c){return function(f){return{tink0:u,tink1:o,tink2:c,tink3:f}}}}})(hn(Sn)(wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(1)(function(){var o=function(c){return q(k)(Nf()(If)(fs()(Ue(_a)(c))(y_)))};return[Nn(Na)(u.tink0)(o(.1)),Nn(Na)(u.tink1)(o(.2)),Nn(Na)(u.tink2)(o(.9)),Nn(Na)(u.tink3)(o(1.8))]}())])}}))})}}};var e1=function(){return b.value}(),yS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(e1)({ai0:W(at(a)(a))(Lt(r)(t)(function(n){return io(Sn)(Gt(li)(Gt(li)(Gt(li)(_(wc)(function(u){return function(o){return function(c){return function(f){return{tink0:u,tink1:o,tink2:c,tink3:f}}}}})(hn(Sn)(wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(hn(Sn)(wt(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return St(n)([re(_t)(G(a)(a)(a))(1)(function(){var o=function(f){return q(k)(Nf()(If)(fs()(Ue(_a)(f))(y_)))},c=function(f){var l=tu(Do)(f)(4);return l===0?u.tink0:l===1?u.tink1:l===2?u.tink2:u.tink3};return er(Oe)(Qn(0)(100))(function(f){var l=Ve(f);return Nn(Na)(c(f))(o(.3+.3*(l*Ri(1.005)(l))))})}())])}}))})}}};var n1=function(){return b.value}(),AS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
  <pre><code>run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> mix $ gain_ 0.8
        [ bandpass_ { frequency: 400.0, q: 1.0 } [ b ]
        , bandpass_ { frequency: 880.0, q: 5.0 } [ b ]
        , bandpass_ { frequency: 1200.0, q: 10.0 } [ b ]
        , bandpass_ { frequency: 2000.0, q: 20.0 } [ b ]
        , bandpass_ { frequency: 3000.0, q: 30.0 } [ b ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(n1)({ai0:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Ca(he(qe)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(.8)([pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:400,q:1})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:880,q:5})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:1200,q:10})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:2e3,q:20})([o]),pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:3e3,q:30})([o])]))}})])}}))})}}};var u1=function(){return b.value}(),kS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fan1 (loopBuf buf bangOn)
      \\b _ -> mix $ gain_ 0.8
        $ 0 .. 40 &lt;#&gt; lcmap toNumber
            \\i -> bandpass_
              { frequency: 200.0 + i * 150.0, q: 30.0 }
              [ b ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(u1)({ai0:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return St(n)([Ca(he(qe)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(re(_t)(G(a)(a)(a))(.8)(er(Oe)(Qn(0)(40))(uo(Ra)(Ve)(function(f){return pn(an(At(yt()(H(H(bt)(_n)()()()({reflectSymbol:function(){return"q"}}))(nn)()()()({reflectSymbol:function(){return"frequency"}})))(dt()())))(G(a)(a)(a))({frequency:200+f*150,q:30})([o])}))))}})])}}))})}}};var i1=function(){return b.value}(),gS=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(i1)({ai0:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return St(n)([Ja(function(o){return re(_t)(G(a)(a)(a))(1)([Nn(Na)(u)(st()),oo(Xr)(G(a)(a)(a))(.1)([re(_t)(G(a)(a)(a))(.6)([o])])])})])}}))})}}};var c1=function(){return b.value}(),l1=function(t){return function(e){return q(t)(Dn(e)(Ta)({p:[1,1,0],o:0,d:10}))}},_1=function(t){return function(e){return q(t)(Dn(e)(Ta)({p:[1,1,0],o:0,d:8}))}},Vc=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return oo(t)(G(a)(a)(a))(u)([re(e)(G(a)(a)(a))(o)([xc(r)(n)(c)(f)])])}}}}}}}},ES=function(t){return function(e){return function(r){return De({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(b.value)(c1)({txt:W(N(a)(a))(br(`dgh d g h i =
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
  ]`)),ai0:W(at(a)(a))(Lt(r)(t)(function(n){return wt(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return St(n)([Ca(Nn(Na)(u)(st()))(function(o){return function(c){return je(nr(a)(a)(a))(Ja(function(f){return re(_t)(G(a)(a)(a))(1)([o,Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.15)(.7)(1500)([Ja(function(l){return Br(_t)(G(a)(a)(a))(1)(l1(k)())([Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.4)(.5)(2500)([f,l])])})]),Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.29)(.85)(2e3)([Ja(function(l){return re(_t)(G(a)(a)(a))(1)([Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.6)(.6)(3500)([f,Ja(function(p){return Br(_t)(G(a)(a)(a))(1)(_1(k)())([Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.75)(.6)(4e3)([l,p]),Vc(Xr)(_t)(Ba)(G(a)(a)(a))(.75)(.55)(3e3)([o])])})])])})])])}))}})])}}))})}}};var s1=function(){return b.value}(),CS=function(t){return function(e){return function(r){return function(n){var u=function(c){return ja(O)(k)(n)(J(j)(e(c))(Tn))},o=Sa(t)(r);return De({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(Cn()(Z)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(b.value)(s1)({hwLink:u(wf.value)})}}}};var v1=function(){return b.value}(),hS=function(t){return function(e){return function(r){return function(n){var u=function(c){return ja(O)(k)(n)(J(j)(e(c))(Tn))},o=Sa(t)(r);return De({reflectType:function(){return`<div>
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
    <li>A function that accepts a reference to this/these node(s) and returns a new <i>mixed</i> node that may or may not contain the input. Mixed nodes are created using the <code>mix</code> function.</li>
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
</div>`}})({reflectType:function(){return"@"}})()()(L()(L()(L()(L()(L()(L()(L()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(b.value)(v1)({intro:W(N(a)(a))(CS(t)(e)(r)(n)),next:u(__.value),code0:W(N(a)(a))(bS(o)(e)(n)),code1:W(N(a)(a))(yS(o)(e)(n)),code2:W(N(a)(a))(AS(o)(e)(n)),code3:W(N(a)(a))(kS(o)(e)(n)),code4:W(N(a)(a))(gS(o)(e)(n)),code5:W(N(a)(a))(ES(o)(e)(n))})}}}};var SS=function(t){return function(e){return function(r){return V("code")(e)(x(t)(r))}}},ob=function(t){return SS(t)(T(g))};var TS=function(t){return function(e){return function(r){return V("pre")(e)(x(t)(r))}}},ib=function(t){return TS(t)(T(g))};var y1=function(){return b.value}(),xS=function(t){return function(e){return function(r){return function(n){var u=J(j)(e(p_.value))(Tn),o=Sa(t)(r);return De({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(L()(Cn()(L()(Z)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(b.value)(y1)({code:W(N(a)(a))(ib(Ee(a)(a))([ob(Ee(a)(a))([br(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:W(at(a)(a))(Lt(n)(o)(function(c){return I(fa)(void 0)})(function(c){return function(f){return St(c)([re(_t)(G(a)(a)(a))(.15)([$f(Hi)(440)(st())])])}})),next:ja(O)(k)(n)(u)})}}}};var FS=ac;var $S=function(){return function(t){return t}};var wS=function(){return function(t){return t}};var fb=function(){function t(){}return t.value=new t,t}();var MS={attr:function(t){return function(e){return v({key:"height",value:R(e)})}}};var cb=function(){function t(){}return t.value=new t,t}();var PS={attr:function(t){return function(e){return v({key:"width",value:R(e)})}}};var lb=function(t){return function(e){return function(r){return V("canvas")(e)(x(t)(r))}}};var _b=function(){function t(){}return t.value=new t,t}(),pb={attr:function(t){return function(e){return v({key:"@canvas-hack@",value:ut(e)})}}};function h_(t){return function(e){return function(){t.fillStyle=e}}}function ws(t){return function(){t.beginPath()}}function Ms(t){return function(){t.fill()}}function sb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function Ps(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var N1=function(){return 2*Ni}(),Jc=function(t){return{o:t.value0+.04,n:t.value1,t:Vi}};var L1=function(){return b.value}(),jc=function(t){return function(e){return function(r){return function(n){return q(t)(co(e)(Ta)({p:[r,n],o:0,d:16}))}}}},W1=function(t){return function(e){return q(t)(Dn(e)(Ta)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},q1=function(t){return function(e){return q(t)(Dn(e)(Ta)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Os=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return function(l){return function(p){return function(m){return u_(t)(G(a)(a)(a))(u)(o)([Br(e)(G(a)(a)(a))(c)(f)([DD(r)(n)(l)(p)(m)])])}}}}}}}}}}},OS=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(f){return function(l){return function(p){return function(m){return u_(t)(G(a)(a)(a))(u)(o)([Br(e)(G(a)(a)(a))(c)(f)([dD(r)(n)(l)(p)(m)])])}}}}}}}}}}},G1=function(t){return function(e){return function(r){return function(n){return q(t)(Nc(e)(Ta)({p:[r,n],o:0,d:16}))}}}},IS=400,mb=Ve(IS),B1=function(){return Ht(Ru)(IS)+"px"}(),RS=600,vb=Ve(RS),U1=function(){return Ht(Ru)(RS)+"px"}(),H1={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},NS=function(t){return function(e){return function(r){return De({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(L()(Z)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(b.value)(L1)({ex1:W(at(a)(a))($u()(ir({reflectSymbol:function(){return"canvas"}})()()(ir({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(ir({reflectSymbol:function(){return"loading"}})()()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())()()()())(b.value)(function(n){return function(u){var o=M(O)(q(k)(void 0))(u.startStop.start),c=function(f){return function(l){return function(p){var m=_(y)(function(s){return new et(s.acTime,s.value)})(Gc(f)(u.slider));return[Kp(Qp(At(yt()(H(H(bt)(jp)()()()({reflectSymbol:function(){return"fftSize"}}))(Jp)()()()({reflectSymbol:function(){return"cb"}})))(dt()())))(nr(a)(a)(a))({cb:function(s){return function(){return ze(new U(s))(p)(),ze(z.value)(p)}},fftSize:Gp.value})(Ca(Nn(Na)(l)(M(O)(st())(_(y)(function(){var s=bs()(xn),d=Ln(vn)(xa(0)(.96)(100)(1.04));return function(i){return s(Jc(d(i)))}}())(m))))(function(s){return function(d){return je(nr(a)(a)(a))(Ja(function(i){return re(_t)(G(a)(a)(a))(1)([s,u_(cD(At(yt()(H(H(bt)(pE)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(Kv)()()()({reflectSymbol:function(){return"delayTime"}})))(dt()())))(G(a)(a)(a))({maxDelayTime:2.5,delayTime:1})(_(y)(function(){var h=Nc()(xn),ct=Ln(vn)(xa(0)(.5)(100)(2.45));return function(mt){return h(Jc(ct(mt)))}}())(m))([Br(_t)(G(a)(a)(a))(.4)(_(y)(function(){var h=Dn()(xn),ct=Ln(vn)(xa(0)(.6)(100)(.9));return function(mt){return h(Jc(ct(mt)))}}())(m))([s])]),Os(Xr)(_t)(Ba)(G(a)(a)(a))(.15)(T(g))(.7)(T(g))(1500)(jc(k)()(1500)(3e3))([Ja(function(h){return Br(_t)(G(a)(a)(a))(1)(W1(k)())([Os(Xr)(_t)(Ba)(G(a)(a)(a))(.4)(T(g))(.5)(T(g))(3e3)(jc(k)()(3e3)(100))([i,h])])})]),Os(Xr)(_t)(Ba)(G(a)(a)(a))(.29)(_(y)(function(){var h=Nc()(xn),ct=Ln(vn)(xa(0)(.1)(100)(.4));return function(mt){return h(Jc(ct(mt)))}}())(m))(.85)(T(g))(2e3)(jc(k)()(2e3)(5e3))([Ja(function(h){return re(_t)(G(a)(a)(a))(1)([Os(Xr)(_t)(Ba)(G(a)(a)(a))(.6)(_(y)(function(){var ct=Nc()(xn),mt=Ln(vn)(xa(0)(.8)(100)(.3));return function(fe){return ct(Jc(mt(fe)))}}())(m))(.6)(T(g))(3500)(jc(k)()(3500)(100))([i,Ja(function(ct){return Br(_t)(G(a)(a)(a))(1)(q1(k)())([OS(Xr)(_t)(lD)(G(a)(a)(a))(.75)(_(y)(function(){var mt=Nc()(xn),fe=Ln(vn)(xa(0)(.9)(100)(.1));return function(jt){return mt(Jc(fe(jt)))}}())(m))(.6)(T(g))(4e3)(jc(k)()(4e3)(200))([h,ct]),OS(Xr)(_t)(lD)(G(a)(a)(a))(.75)(G1(k)()(.75)(.2))(.55)(T(g))(200)(jc(k)()(200)(4e3))([s])])})])])})])])}))}}))]}}};return x(N(a)(a))(Je(Ee(a)(a))([lb(N(a)(a))(M(O)(Ir(Jt)(g)(q(k))([Q(PS)(cb.value)(U1),Q(MS)(fb.value)(B1),Q(Ik)(qt.value)("width: 100%;"),Q(pb)(_b.value)(function(f){return function(){return h_(f)("black")(),Ps(f)({width:vb,height:mb,x:0,y:0})(),void 0}})]))(_(y)(function(f){return Q(pb)(_b.value)(function(l){return function(){return h_(l)("black")(),Ps(l)({width:vb,height:mb,x:0,y:0})(),h_(l)("rgba(255,255,255,0.2)")(),hi(f)(function(m){return function(){return ws(l)(),sb(l)({end:N1,radius:m.value1*40,start:0,x:m.value0.x*vb,y:m.value0.y*mb,useCounterClockwise:!1})(),Ms(l)()}})()}})})(u.canvas)))(ee),_o(N(a)(a))(Ir(Jt)(g)(q(k))([Q(Io)(lo.value)("range"),Q(Ki)(Di.value)("0"),Q(Qi)(vi.value)("100"),Q(Zi)(bi.value)("1"),Q(tf)(yi.value)("50"),Q(Ok)(qt.value)("width: 100%;"),Q(Yi)(di.value)(Ze(function(){var f=lr(tt)(rr)(Hf(Le)(Rf)(n.slider)),l=Gn(va)(Rc);return function(p){return f(l(zc(p)))}}()))]))(ee),sn(Ee(a)(a))(Jr(Jt)(g)([q(k)(Q(_c)(qt.value)("width:100%; padding:1.0rem;")),Ir(Jt)(g)(_(y)(function(){var f=Q(pr)(cr.value);return function(l){return f(Ze(E(l)))}}()))([X(y)(u.startStop.loading)(I(tt)(void 0)),er(y)(u.startStop.stop)(function(f){return J(j)(f)(J(j)(t(I(tt)(void 0)))(n.startStop.start(void 0)))}),er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(f){return f.value0})(r)))(X(y)(o)(K(Y))))(function(f){return function(){f(),n.startStop.loading(void 0)();var p=ye(z.value)(),m=fo(rt(Or)(Jn(Pr))(function(s){return rt(Or)(Fu(Pr)(s))(function(d){return rt(Or)(_(ci)(wS())(aC(Sn)(FS)(wt(s))($S()(H1))))(function(i){return rt(Or)(_r(Pr)(Ul(0)(5e4)))(function(h){var ct=Cp(iv(ma(hA(i.pluck0))(oc(Mm(Pm()(i))))))({newSeed:Ep(h),size:4});return _r(Pr)(function(){var fe=Un(zo)(tt)(function(it){return function(){var se=Ga(),It=Ga();return{x:se,y:It}}})(Qn(0)(127))(),jt=St(s)(c(s)(ct)(p))(),ge=kt(Ji)(function(it){return function(){var se=Ce(p)();return yu(tt)(rr)(se)(function(It){return function(){var Lr=D_(It)(),Wr=_(C)(function(){var Ro=bl(fe),Qa=_(Oe)(function(un){return function(No){return No/255}(un)});return function(un){return Ro(Qa(un))}}())(ps(_s)(Lr))();return n.canvas(Wr)(),void 0}})()}})(),Nr=J(j)(J(j)(J(j)(jt)(d))(mn(or)(s)))(ge);return n.startStop.stop(Nr)(),Nr})})})})}))();return t(function(){return n.startStop.start(void 0)(),wo(_i(m))()})(),void 0}})])]))([rn(Jr(Jt)(g)([_(y)(E("Turn off"))(u.startStop.stop),_(y)(E("Turn on"))(o),_(y)(E("Loading..."))(u.startStop.loading)]))])]))}}))})}}};var V1=function(){return b.value}(),LS=function(t){return function(e){return function(r){return function(n){var u=Sa(t)(r);return Nt({reflectType:function(){return`<div>
  <h1>Wags</h1>

  <h3>A web-audio framework written in PureScript</h3>

  <p>Hi! You've found <a href="https://github.com/mikesol/purescript-wags">Wags</a>.</p>

  <p>Wags is a web-audio framework designed for interactive media and games. Events like mouse clicks, MIDI notes and tweening frames are streamed to an audio rendering engine and, in response to these events, sound happens.</p>

  <h2>Why?</h2>

  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API">Web Audio API</a> is an amazing piece of technology. It is clear, concise, straightforward and ergonomic. So why build a framework on top of it?</p>

  <p>As audio projects become more and more ambitious, a need emerges for powerful abstractions to handle browser events and scheduling. Wags aims to tackle this problem through a small set of <a href="https://en.wikipedia.org/wiki/Functional_reactive_programming">FRP</a>-based abstractions. In doing so, it aims to be concise, expressive, and as fast as manually-optimized hand-written JavaScript.</p>

  <h2>How does it sound?</h2>

  <p>Here's a small example in Wags that, when you turn it on, emits a single sound and then uses feedback loops to create long tail. You can use the slider to change the properties of the tail in real time.</p>

  ~ex~

  <p>By the end of this documentation, you'll know all of the concepts you need to create interactive audio like the example above.</p>

  <p>If you'd like to use this documentation as a springboard for your own work, it can be found <a href="https://github.com/mikesol/purescript-wags/tree/main/examples/docs">here</a>.</p>

  <p>And now, without further ado, let's write a small <a ~next~ style="cursor:pointer;">hello world \xE0 la wags</a>!</p>
</div>`}})()()(L()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(V1)({next:ja(O)(k)(n)(J(j)(e(wf.value))(Tn)),ex:W(N(a)(a))(NS(u)(e)(n))})}}}};var j1=function(){return b.value}(),WS=function(t){return function(e){return function(r){return function(n){return Nt({reflectType:function(){return`<div>
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
</div>`}})()()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(j1)({next:q(k)(Q(pr)(cr.value)(Ze(E(J(j)(e(Zp.value))(Tn)))))})}}}};var Q1=function(){return b.value}(),qS=function(t){return function(e){return function(r){return function(n){return Nt({reflectType:function(){return`<div>
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
</div>`}})()()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(Q1)({next:q(k)(Q(pr)(cr.value)(Ze(E(J(j)(e(ts.value))(Tn)))))})}}}};var Y1=function(){return b.value}(),GS=function(t){return function(e){return function(r){return function(n){return Nt({reflectType:function(){return`<div>
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
</div>`}})()()(Cn()(Z)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(Y1)({next:q(k)(Q(pr)(cr.value)(Ze(E(J(j)(e(Mf.value))(Tn)))))})}}}};var tL=function(){return b.value}(),BS=function(t){return function(e){return function(r){return function(n){return De({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(Z)(b.value)(tL)({})}}}};var rL=`module Main where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf, oneOfMap)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\\))
import Deku.Attribute (attr, cb, (:=))
import Deku.Control (blank, switcher, text, text_)
import Deku.Core (Element)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Random as Random
import FRP.Behavior (Behavior, behavior, sampleBy)
import FRP.Event (create, fold, makeEvent, subscribe)
import FRP.Event.Class (bang)
import FRP.Event.Time (delay)
import FRP.Event.VBus (V, vbus)
import Type.Proxy (Proxy(..))
import WAGS.Control (gain_, playBuf)
import WAGS.Core (Channel(..))
import WAGS.Interpret (bracketCtx, decodeAudioDataFromUri)
import WAGS.Parameter (bangOn)
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
    -> Element lock payload
  scene = maybe (D.div_ [ text_ "Loading..." ]) \\buffer ->
    D.div_ $ vbus (Proxy :: _ UIEvents) \\push event -> do
      let
        startE = bang unit <|> event.startStop.start
        sl = sampleBy (/\\) random
          $ fold (\\_ b -> b + 1) event.slider 0
        music = run2_
          [ gain_ 1.0 $ map
              ( \\i ->
                  oneOf
                    [ bang $ Sound $ playBuf
                        { buffer: buffer, playbackRate: 0.7 + (fst i) * 2.0 }
                        bangOn
                    , delay 5000 $ bang $ Silence
                    ]
              )
              sl
          ]
      D.div_
        [ D.div_
            [ text_ "Slide me!"
            , D.input
                ( oneOfMap bang
                    [ D.Xtype := "range"
                    , D.Min := "0"
                    , D.Max := "100"
                    , D.Step := "1"
                    , D.Value := "50"
                    , D.OnInput := cb (const (push.slider unit))
                    ]
                )
                blank
            ]
        , D.button
            ( oneOfMap (map (attr D.OnClick <<< cb <<< const))
                [ startE $> (music >>= push.startStop.stop)
                , event.startStop.stop <#>
                    (_ *> push.startStop.start unit)
                ]
            )
            [ text $ oneOf
                [ startE $> "Turn on"
                , event.startStop.stop $> "Turn off"
                ]
            ]
        ]
`,nL=Xa(function(t){return Pt(function(e){return kt(t)(function(r){return function(){var u=Ga();return e(r(u))()}})})}),aL=function(){return b.value}(),uL="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",US=function(t){return function(e){return function(r){return De({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>Sound (Node outputChannels lock payload)</code> to create a subgraph and <code>Silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>Silence</code> event. Note that, once you turn a subraph off with <code>Silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(L()(L()(Z)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(b.value)(aL)({txt:W(N(a)(a))(br(rL)),ex1:W(at(a)(a))($u()(ir({reflectSymbol:function(){return"slider"}})()()(wu({reflectSymbol:function(){return"startStop"}})()()()(ir({reflectSymbol:function(){return"loading"}})()()(ir({reflectSymbol:function(){return"start"}})()()(ir({reflectSymbol:function(){return"stop"}})()()(Wn)()()()())()()()())()()()())(Wn)()()()())()()()())(b.value)(function(n){return function(u){var o=M(O)(q(k)(void 0))(u.startStop.start),c=mi(k)(et.create)(nL)(Cu(k)(function(l){return function(p){return p+1|0}})(u.slider)(0)),f=function(l){return[re(_t)(Xg(a)(a)(a))(1)(_(y)(function(p){return Jr(Jt)(g)([q(k)(new Vv(Nn(a_(At(yt()(H(H(bt)(uE)()()()({reflectSymbol:function(){return"playbackRate"}}))(e_)()()()({reflectSymbol:function(){return"buffer"}})))(dt()())))({buffer:l,playbackRate:.7+Pa(p)*2})(st()))),uS(5e3)(q(k)(Jv.value))])})(c))]};return x(N(a)(a))(Je(Ee(a)(a))([Je(Ee(a)(a))([br("Slide me!"),_o(N(a)(a))(Ir(Jt)(g)(q(k))([Q(Io)(lo.value)("range"),Q(Ki)(Di.value)("0"),Q(Qi)(vi.value)("100"),Q(Zi)(bi.value)("1"),Q(tf)(yi.value)("50"),Q(Yi)(di.value)(Ze(E(n.slider(void 0))))]))(ee)]),sn(Ee(a)(a))(Ir(Jt)(g)(_(y)(function(){var l=Q(pr)(cr.value);return function(p){return l(Ze(E(p)))}}()))([X(y)(u.startStop.loading)(I(tt)(void 0)),er(y)(u.startStop.stop)(function(l){return J(j)(l)(J(j)(t(I(tt)(void 0)))(n.startStop.start(void 0)))}),er(y)(En(k)(M(O)(q(k)(I(tt)(void 0)))(_(y)(function(l){return l.value0})(r)))(X(y)(o)(K(Y))))(function(l){return function(){l(),n.startStop.loading(void 0)();var m=fo(rt(Or)(Jn(Pr))(function(s){return rt(Or)(Fu(Pr)(s))(function(d){return rt(Or)(wt(s)(uL))(function(i){return _r(Pr)(function(){var ct=ls(f(i))(),mt=J(j)(J(j)(ct)(d))(mn(or)(s));return n.startStop.stop(mt)(),mt})})})}))();return t(function(){return n.startStop.start(void 0)(),wo(_i(m))()})(),void 0}})]))([rn(Jr(Jt)(g)([_(y)(E("Turn off"))(u.startStop.stop),_(y)(E("Turn on"))(o)]))])]))}}))})}}};var iL=function(){return b.value}(),HS=function(t){return function(e){return function(r){return function(n){var u=Sa(t)(r);return Nt({reflectType:function(){return`<div>
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
</div>`}})()()(L()(L()(Z)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(iL)({appl:W(at(a)(a))(us("\u{1F44F}")(n)(u)(function(o){return wt(o)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(o){return function(c){return St(o)([re(_t)(G(a)(a)(a))(1)([he(qe)(c)(st())])])}})),suby:W(N(a)(a))(US(u)(e)(n))})}}}};var cL=function(t){return function(e){var r=function(u){var o=function(c){if(c instanceof l_)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(LS(u.setCancellation)(u.setPage))));if(c instanceof wf)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(xS(u.setCancellation)(u.setPage))));if(c instanceof p_)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(hS(u.setCancellation)(u.setPage))));if(c instanceof __)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(Eh(u.setCancellation)(u.setPage))));if(c instanceof Zp)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(GS(u.setCancellation)(u.setPage))));if(c instanceof Mf)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(dS(u.setCancellation)(u.setPage))));if(c instanceof s_)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(fS(u.setCancellation)(u.setPage))));if(c instanceof ts)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(BS(u.setCancellation)(u.setPage))));if(c instanceof dC)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(WS(u.setCancellation)(u.setPage))));if(c instanceof m_)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(HS(u.setCancellation)(u.setPage))));if(c instanceof bC)return Je(at(a)(a))(_(y)(x(N(a)(a)))(on(qS(u.setCancellation)(u.setPage))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 142, column 5 - line 142, column 78): "+[c.constructor.name])};return o(u.page)},n=Cu(k)(function(u){if(u instanceof Mc)return function(o){return{prevPage:new U(o.curPage),curPage:u.value0,cancel:o.cancel,pageChange:!0}};if(u instanceof gD)return function(o){return{cancel:u.value0,pageChange:!1,curPage:o.curPage,prevPage:o.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 132, column 7 - line 134, column 75): "+[u.constructor.name])})(e)({prevPage:z.value,curPage:l_.value,cancel:I(tt)(void 0),pageChange:!0});return[Je(Ee(a)(a))(_(Oe)(function(u){return Av(Ee(a)(a))([yv(Ee(a)(a))(M(O)(Ir(Jt)(g)(q(k))([Q(pr)(cr.value)(Ze(E(t(new Mc(u.value0))))),Q(Rk)(qt.value)("cursor:pointer;")]))(_(y)(function(o){return Q(pr)(cr.value)(Ze(E(function(){return o.cancel(),t(new Mc(u.value0))()})))})(Cl(lu)(function(){var o=nu(wa);return function(c){return o(function(f){return f.pageChange}(c))}}())(n))))([br(u.value1.value0)]),sc(Ee(a)(a))(q(k)(Q(Ip)(qt.value)(function(){return u.value1.value1?"":"display:none;"}())))([br(" | ")])])})([new et(l_.value,new et("Home",!0)),new et(wf.value,new et("Hello world",!0)),new et(p_.value,new et("Array, fan, and fix",!0)),new et(__.value,new et("Audio units",!0)),new et(Mf.value,new et("Events",!0)),new et(s_.value,new et("State",!0)),new et(m_.value,new et("Subgraphs",!1))])),Je(Mk(a)(a))(wk(function(u){return r({page:u.curPage,setPage:function(o){return t(Mc.create(o))},setCancellation:function(o){return t(gD.create(o))}})})(Cl(lu)(function(u){return u.pageChange})(n)))]}};var zS=function(){var e=rt(Le)(rt(Le)(si)(Wd))(UC)();return yu(tt)(rr)(_(xr)(HC)(e))(function(r){return function(){var u=gv(),o=Ql(),c=Pk(r)(cL(o.push)(o.event))(Qk);return Be(C)(kt(c)(function(f){return f(u)}))(),o.push(new Mc(l_.value))()}})()};zS();
