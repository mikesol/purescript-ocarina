var Cb=function(t){return function(e){for(var r=e.length,n=new Array(r),u=0;u<r;u++)n[u]=t(e[u]);return n}};var zo={compose:function(t){return function(e){return function(r){return t(e(r))}}}},Ao=function(t){return t.compose};var Y=function(t){return t.identity},Z={identity:function(t){return t},Semigroupoid0:function(){return zo}};var nr=!0;var ht=function(t){return function(e){return function(r){return t(r)(e)}}},S=function(t){return function(e){return t}};var zf=function(t){return function(e){return e(t)}},tl=function(t){return function(e){return t(e)}};var D=function(){function t(){}return t.value=new t,t}();var _=function(t){return t.map},ar=function(t){return function(e){return function(r){return _(t)(r)(e)}}},Le=function(t){return _(t)(S(void 0))},j=function(t){return function(e){return function(r){return _(t)(S(r))(e)}}},I_=function(t){return function(e){return _(t)(S(e))}};var ja={map:Ao(zo)},Pe={map:Cb},Bs=function(t){return function(e){return function(r){return _(t)(function(n){return n(r)})(e)}}};var Sb=function(t){return function(e){return t.length===0?e:e.length===0?t:t.concat(e)}};var Tr=function(t){return t.reflectSymbol};var R_=function(t){return function(e){return{}.hasOwnProperty.call(e,t)}},Xa=function(t){return function(e){return e[t]}},qu=function(t){return function(e){return function(r){var n={};for(var u in r)({}).hasOwnProperty.call(r,u)&&(n[u]=r[u]);return n[t]=e,n}}};var Tb={append:function(t){return function(e){return void 0}}};var uu={append:Sb};var St=function(t){return t.append},Hs=function(t){return{append:function(e){return function(r){return function(n){return St(t)(e(n))(r(n))}}}}};var P=function(t){return t.alt};var xb=function(t){return function(e){for(var r=t.length,n=e.length,u=new Array(r*n),o=0,c=0;c<r;c++)for(var i=t[c],l=0;l<n;l++)u[o++]=i(e[l]);return u}};var N_={apply:xb,Functor0:function(){return Pe}},Vt=function(t){return t.apply};var H=function(t){return function(e){return function(r){return Vt(t)(_(t.Functor0())(S(Y(Z)))(e))(r)}}},ea=function(t){return function(e){return function(r){return function(n){return Vt(t)(_(t.Functor0())(e)(r))(n)}}}};var L=function(t){return t.pure};var En=function(t){return function(e){return function(r){if(e)return r;if(!e)return L(t)(void 0);throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): "+[e.constructor.name,r.constructor.name])}}},el=function(t){return function(e){return function(r){return Vt(t.Apply0())(L(t)(e))(r)}}};var Fb=function(t){return function(e){for(var r=[],n=0,u=t.length;n<u;n++)Array.prototype.push.apply(r,e(t[n]));return r}};var Vf=function(t){return t.discard};var Vo={bind:Fb,Apply0:function(){return N_}},nt=function(t){return t.bind},zn=function(t){return ht(nt(t))};var Jf=function(t){return function(e){return function(r){return function(n){return nt(t)(e(n))(r)}}}};var Jo={discard:function(t){return nt(t)}};var ra=function(t){return function(e){return nt(t)(e)(Y(Z))}};var sn=function(t){return function(e){for(var r=t>e?-1:1,n=new Array(r*(e-t)+1),u=t,o=0;u!==e;)n[o++]=u,u+=r;return n[o]=u,n}},f0=function(t){return function(e){if(t<1)return[];var r=new Array(t);return r.fill(e)}},c0=function(t){return function(e){for(var r=[],n=0,u=0;u<t;u++)r[n++]=e;return r}},Js=typeof Array.prototype.fill=="function"?f0:c0,l0=function(){function t(u,o){this.head=u,this.tail=o}var e={};function r(u){return function(o){return new t(u,o)}}function n(u){for(var o=[],c=0,i=u;i!==e;)o[c++]=i.head,i=i.tail;return o}return function(u){return function(o){return n(u(r)(e)(o))}}}(),Gu=function(t){return t.length};var $b=function(t){return function(e){return function(r){return function(n){for(var u=0,o=n.length;u<o;u++)if(r(n[u]))return t(u);return e}}}};var wb=function(t){return function(e){return function(r){return function(n){if(r<0||r>=n.length)return e;var u=n.slice();return u.splice(r,1),t(u)}}}};var _0=function(){function t(e,r,n,u,o,c){var i,l,p,s,m,v,f;for(i=o+(c-o>>1),i-o>1&&t(e,r,u,n,o,i),c-i>1&&t(e,r,u,n,i,c),l=o,p=i,s=o;l<i&&p<c;)m=u[l],v=u[p],f=r(e(m)(v)),f>0?(n[s++]=v,++p):(n[s++]=m,++l);for(;l<i;)n[s++]=u[l++];for(;p<c;)n[s++]=u[p++]}return function(e){return function(r){return function(n){var u;return n.length<2?n:(u=n.slice(0),t(e,r,u,n.slice(0),0,n.length),u)}}}}();var rl=function(t){return function(e){return function(r){for(var n=e.length<r.length?e.length:r.length,u=new Array(n),o=0;o<n;o++)u[o]=t(e[o])(r[o]);return u}}};var Pb=function(t){return function(e){return t[e]}};var Bu=function(t){return function(e){return function(r){return nt(t.Bind1())(e)(function(n){return nt(t.Bind1())(r)(function(u){return L(t.Applicative0())(n(u))})})}}};var s0=String.fromCharCode(65535),m0=String.fromCharCode(0),v0=Number.POSITIVE_INFINITY,D0=Number.NEGATIVE_INFINITY;var Ob=function(t){return function(e){return function(r){return function(n){return function(u){return n<u?t:n===u?e:r}}}}};var Ib=Ob,Rb=Ob;var Nb=function(t){return function(e){return t===e}};var Lb=Nb,Wb=Nb;var al={eq:Wb},Fi={eq:Lb};var fe=function(t){return t.eq};var Yt=function(){function t(){}return t.value=new t,t}(),ye=function(){function t(){}return t.value=new t,t}(),Ae=function(){function t(){}return t.value=new t,t}();var qb=function(t){return function(e){return t-e|0}},Gb=function(t){return function(e){return t-e}};var Bb=function(t){return function(e){return t+e|0}},Ub=function(t){return function(e){return t*e|0}},Hb=function(t){return function(e){return t+e}},zb=function(t){return function(e){return t*e}};var ya=function(t){return t.zero};var Aa={add:Hb,zero:0,mul:zb,one:1},jo={add:Bb,zero:0,mul:Ub,one:1};var ka=function(t){return t.one};var wn=function(t){return t.mul};var Ve=function(t){return t.add};var Eu=function(t){return t.sub};var ff={sub:Gb,Semiring0:function(){return Aa}},Vb={sub:qb,Semiring0:function(){return jo}};var ul=function(t){return function(e){return Eu(t)(ya(t.Semiring0()))(e)}};var Ra=function(){return{compare:Rb(Yt.value)(Ae.value)(ye.value),Eq0:function(){return al}}}(),Zr=function(){return{compare:Ib(Yt.value)(Ae.value)(ye.value),Eq0:function(){return Fi}}}();var Kt=function(t){return t.compare};var Jb=function(t){return function(e){return function(r){var n=Kt(t)(e)(r);return!(n instanceof Yt)}}};var Su=function(t){return function(e){return function(r){var n=Kt(t)(e)(r);if(n instanceof Yt)return r;if(n instanceof Ae||n instanceof ye)return e;throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): "+[n.constructor.name])}}};var jb=function(t){return function(e){return function(r){var n=Jb(t)(r)(ya(e.Semiring0()));return n?r:ul(e)(r)}}};var Pn=function(t){return t.top};var cf={top:2147483647,bottom:-2147483648,Ord0:function(){return Zr}};var Mn=function(t){return t.bottom};var Qb=function(t){return t.toString()},Kb=function(t){var e=t.toString();return isNaN(e+".0")?e:e+".0"};var Xf={show:Kb},Hu={show:Qb};var jt=function(t){return t.show};var z=function(){function t(){}return t.value=new t,t}(),B=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var jr=function(t){return function(e){return function(r){if(r instanceof z)return t;if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}};var wr={map:function(t){return function(e){return e instanceof B?new B(t(e.value0)):z.value}}};var ga=function(t){return jr(t)(Y(Z))},Vn=function(){return function(t){if(t instanceof B)return t.value0;throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): "+[t.constructor.name])}};var wi={apply:function(t){return function(e){if(t instanceof B)return _(wr)(t.value0)(e);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): "+[t.constructor.name,e.constructor.name])}},Functor0:function(){return wr}},Ea={bind:function(t){return function(e){if(t instanceof B)return e(t.value0);if(t instanceof z)return z.value;throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): "+[t.constructor.name,e.constructor.name])}},Apply0:function(){return wi}};var go=function(){return{pure:B.create,Apply0:function(){return wi}}}();var Mt=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Ot=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var _f={map:function(t){return function(e){if(e instanceof Mt)return new Mt(e.value0);if(e instanceof Ot)return new Ot(t(e.value0));throw new Error("Failed pattern match at Data.Either (line 31, column 1 - line 31, column 52): "+[e.constructor.name])}}};var Na=function(t){return function(e){return function(r){if(r instanceof Mt)return t(r.value0);if(r instanceof Ot)return e(r.value0);throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},W_=function(){return Na(S(z.value))(B.create)}();var ou=function(t){return t};var Eo={map:function(t){return function(e){return t(e)}}};var Yb={apply:function(t){return function(e){return t(e)}},Functor0:function(){return Eo}},O0={bind:function(t){return function(e){return e(t)}},Apply0:function(){return Yb}},Ys={pure:ou,Apply0:function(){return Yb}},zu={Applicative0:function(){return Ys},Bind1:function(){return O0}};var Zb=function(t){return Math.min(Math.abs(t),2147483647)},ty=function(t){return function(e){return e===0?0:e>0?Math.floor(t/e):-Math.floor(t/-e)}},ey=function(t){return function(e){if(e===0)return 0;var r=Math.abs(e);return(t%r+r)%r}},ry=function(t){return function(e){return t/e}};var ny={Ring0:function(){return ff}},ay={Ring0:function(){return Vb}};var iu=function(t){return t.mod};var ll={degree:function(t){return 1},div:ry,mod:function(t){return function(e){return 0}},CommutativeRing0:function(){return ny}},Co={degree:Zb,div:ty,mod:ey,CommutativeRing0:function(){return ay}},Vu=function(t){return t.div};var Xo={mempty:void 0,Semigroup0:function(){return Tb}};var De=function(t){return t.mempty},Pi=function(t){return{mempty:function(e){return De(t)},Semigroup0:function(){return Hs(t.Semigroup0())}}};var Zs=function(t){return function(){return t}},uy=function(t){return function(e){return function(){return e(t())()}}};var Mi=function(t){return function(e){return function(){for(var r=0,n=t.length;r<n;r++)e(t[r])()}}};var oy=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}},pf={Applicative0:function(){return tt},Bind1:function(){return Ge}},Ge={bind:uy,Apply0:function(){return tm(0)}},tt={pure:Zs,Apply0:function(){return tm(0)}},iy=oy("functorEffect","Effect",function(){return{map:el(tt)}}),tm=oy("applyEffect","Effect",function(){return{apply:Bu(pf),Functor0:function(){return iy(0)}}}),C=iy(20),J=tm(23),fy=function(t){return{append:ea(J)(St(t))}},So=function(t){return{mempty:Zs(De(t)),Semigroup0:function(){return fy(t.Semigroup0())}}};var cy=function(t){return function(){return{value:t}}};var Me=function(t){return function(){return t.value}},ly=function(t){return function(e){return function(){var r=t(e.value);return e.value=r.state,r.value}}},je=function(t){return function(e){return function(){e.value=t}}};var ge=cy,q0=ly,Oi=function(t){return q0(function(e){var r=t(e);return{state:r,value:r}})},na=function(t){return function(e){return Le(C)(Oi(t)(e))}};var V0=function(){function t(e,r,n,u,o,c){var i,l,p,s,m,v,f;for(i=o+(c-o>>1),i-o>1&&t(e,r,u,n,o,i),c-i>1&&t(e,r,u,n,i,c),l=o,p=i,s=o;l<i&&p<c;)m=u[l],v=u[p],f=r(e(m)(v)),f>0?(n[s++]=v,++p):(n[s++]=m,++l);for(;l<i;)n[s++]=u[l++];for(;p<c;)n[s++]=u[p++]}return function(e){return function(r){return function(n){return function(){return n.length<2||t(e,r,n,n.slice(0),0,n.length),n}}}}}();var yy=function(t){return function(e){return t&&e}},Ay=function(t){return function(e){return t||e}},ky=function(t){return!t};var lu=function(t){return t.not};var Yf=function(t){return t.disj},Wa={ff:!1,tt:!0,implies:function(t){return function(e){return Yf(Wa)(lu(Wa)(t))(e)}},conj:yy,disj:Ay,not:ky};var Ey=function(t){return function(e){return function(r){for(var n=e,u=r.length,o=u-1;o>=0;o--)n=t(r[o])(n);return n}}},Cy=function(t){return function(e){return function(r){for(var n=e,u=r.length,o=0;o<u;o++)n=t(n)(r[o]);return n}}};var T=function(t){return t.empty};var rt=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),Zf=function(t){return function(e){return t(e.value0)(e.value1)}};var tn=function(t){return t.value1};var Qo={map:function(t){return function(e){return new rt(e.value0,t(e.value1))}}};var Ga=function(t){return t.value0};var _t=function(t){return t};var Ca=function(){return _t};var en=Ca,In=Ca;var lm=function(){return function(){return function(t){return Ca()}}};var ir=function(t){return t.foldr};var tr=function(t){return function(e){return ir(t)(P(e.Alt0()))(T(e))}},Lr=function(t){return function(e){return function(r){return ir(t)(function(){var n=P(e.Alt0());return function(u){return n(r(u))}}())(T(e))}}},mr=function(t){return function(e){return function(r){return ir(e)(function(){var n=H(t.Apply0());return function(u){return n(r(u))}}())(L(t)(void 0))}}},aa=function(t){return function(e){return ht(mr(t)(e))}},V_=function(t){return function(e){return mr(t)(e)(Y(Z))}},gr=function(t){return t.foldl};var ur={foldr:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof B)return t(r.value0)(e);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof z)return e;if(r instanceof B)return t(e)(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[t.constructor.name,e.constructor.name,r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof z)return De(t);if(r instanceof B)return e(r.value0);throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): "+[e.constructor.name,r.constructor.name])}}}};var Fy=function(t){return function(e){return function(r){return ir(t)(function(n){return function(u){return St(e.Semigroup0())(r(n))(u)}})(De(e))}}},wt={foldr:Ey,foldl:Cy,foldMap:function(t){return Fy(wt)(t)}},J_=function(t){return function(e){return function(r){return gr(t)(function(n){return function(u){return St(e.Semigroup0())(n)(r(u))}})(De(e))}}},Cn=function(t){return t.foldMap};var Ko=function(t){return function(e){return Cn(t)(e)(Y(Z))}};var $y=function(){function t(u){return[u]}function e(u){return function(o){return[u,o]}}function r(u){return function(o){return function(c){return[u,o,c]}}}function n(u){return function(o){return u.concat(o)}}return function(u){return function(o){return function(c){return function(i){return function(l){function p(s,m){switch(m-s){case 0:return c([]);case 1:return o(t)(i(l[s]));case 2:return u(o(e)(i(l[s])))(i(l[s+1]));case 3:return u(u(o(r)(i(l[s])))(i(l[s+1])))(i(l[s+2]));default:var v=s+Math.floor((m-s)/4)*2;return u(o(n)(p(s,v)))(p(v,m))}}return p(0,l.length)}}}}}}();var Jn=function(t){return t.traverse};var Wy=function(t){return function(e){return Jn(t)(e)(Y(Z))}},Yo={traverse:function(t){return $y(Vt(t.Apply0()))(_(t.Apply0().Functor0()))(L(t))},sequence:function(t){return Wy(Yo)(t)},Functor0:function(){return Pe},Foldable1:function(){return wt}};var gl=function(){return rl(rt.create)}();var Cm=function(){return Pb};var Vy=function(t){return[t]};var Jy=function(){return $b(B.create)(z.value)}();var Sm=function(){return wb(B.create)(z.value)}(),hm=function(t){return function(e){return function(r){return r.length===0?[]:jr(r)(function(n){return Vn()(Sm(n)(r))})(Jy(t(e))(r))}}};var ac=function(t){return function(e){return St(uu)([t])(e)}};var jy=function(t){return function(e){for(var r=e.length,n=Array(r),u=0;u<r;u++)n[u]=t(u)(e[u]);return n}};var Xu=function(t){return t.mapWithIndex};var mf={mapWithIndex:jy,Functor0:function(){return Pe}};var To=function(t){return t.foldrWithIndex};var Ku=function(t){return t.foldlWithIndex};var ti=function(t){return t.foldMapWithIndex};var Ni=function(t){return t.traverseWithIndex};var Yu=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var tp=function(t){return function(e){return new Yu(e,T(t))}};var Pr=function(){function t(){}return t.value=new t,t}(),ce=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}(),ep=function(t){return t},gx=function(t){return new ce(t.value0,t.value1)};var Ex=function(t){var e=function(r){return function(n){var u=r,o=!1,c;function i(l,p){if(p instanceof ce&&p.value1 instanceof ce&&p.value1.value1 instanceof ce){u=new ce(p,l),n=p.value1.value1.value1;return}var s=function(v){return v instanceof ce&&v.value1 instanceof ce&&v.value1.value1 instanceof Pr?new ce(t(v.value0),new ce(t(v.value1.value0),Pr.value)):v instanceof ce&&v.value1 instanceof Pr?new ce(t(v.value0),Pr.value):Pr.value},m=function(v){return function(f){var b=v,ct=!1,vt;function Qt(ee,xe){if(ee instanceof ce&&ee.value0 instanceof ce&&ee.value0.value1 instanceof ce&&ee.value0.value1.value1 instanceof ce){b=ee.value1,f=new ce(t(ee.value0.value0),new ce(t(ee.value0.value1.value0),new ce(t(ee.value0.value1.value1.value0),xe)));return}return ct=!0,xe}for(;!ct;)vt=Qt(b,f);return vt}};return o=!0,m(l)(s(p))}for(;!o;)c=i(u,n);return c}};return e(Pr.value)},rp={map:Ex};var Ba={foldr:function(t){return function(e){var r=function(){var u=function(o){return function(c){var i=o,l=!1,p;function s(m,v){if(v instanceof Pr)return l=!0,m;if(v instanceof ce){i=new ce(v.value0,m),c=v.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[m.constructor.name,v.constructor.name])}for(;!l;)p=s(i,c);return p}};return u(Pr.value)}(),n=gr(Ba)(ht(t))(e);return function(u){return n(r(u))}}},foldl:function(t){var e=function(r){return function(n){var u=r,o=!1,c;function i(l,p){if(p instanceof Pr)return o=!0,l;if(p instanceof ce){u=t(l)(p.value0),n=p.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[p.constructor.name])}for(;!o;)c=i(u,n);return c}};return e},foldMap:function(t){return function(e){return gr(Ba)(function(r){var n=St(t.Semigroup0())(r);return function(u){return n(e(u))}})(De(t))}}};var El={append:function(t){return function(e){return ir(Ba)(ce.create)(e)(t)}}};var xm={append:function(t){return function(e){return new Yu(t.value0,St(El)(t.value1)(gx(e)))}}};var Yy={alt:St(El),Functor0:function(){return rp}},Fm=function(){return{empty:Pr.value,Alt0:function(){return Yy}}}();var aA=function(t){return t()};var uA=function(t){throw new Error(t)};var oA=function(){return uA};var Hx=aA,pu=function(t){return Hx(function(){return oA()(t)})};var Jt=function(){function t(){}return t.value=new t,t}(),_e=function(){function t(e,r,n,u){this.value0=e,this.value1=r,this.value2=n,this.value3=u}return t.create=function(e){return function(r){return function(n){return function(u){return new t(e,r,n,u)}}}},t}(),Be=function(){function t(e,r,n,u,o,c,i){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c,this.value6=i}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return new t(e,r,n,u,o,c,i)}}}}}}},t}(),Li=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),ai=function(){function t(e,r,n){this.value0=e,this.value1=r,this.value2=n}return t.create=function(e){return function(r){return function(n){return new t(e,r,n)}}},t}(),Wi=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),Fo=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),qi=function(){function t(e,r,n,u,o,c){this.value0=e,this.value1=r,this.value2=n,this.value3=u,this.value4=o,this.value5=c}return t.create=function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return new t(e,r,n,u,o,c)}}}}}},t}(),ap=function(){function t(e,r,n,u){this.value0=e,this.value1=r,this.value2=n,this.value3=u}return t.create=function(e){return function(r){return function(n){return function(u){return new t(e,r,n,u)}}}},t}();var fA=function(t){return function(e){return new _e(Jt.value,t,e,Jt.value)}};var Qx=function(t){return function(e){var r=Kt(t),n=function(u){var o=!1,c;function i(l){if(l instanceof Jt)return o=!0,z.value;if(l instanceof _e){var p=r(e)(l.value1);if(p instanceof Ae)return o=!0,new B(l.value2);if(p instanceof Yt){u=l.value0;return}u=l.value3;return}if(l instanceof Be){var s=r(e)(l.value1);if(s instanceof Ae)return o=!0,new B(l.value2);var m=r(e)(l.value4);if(m instanceof Ae)return o=!0,new B(l.value5);if(s instanceof Yt){u=l.value0;return}if(m instanceof ye){u=l.value6;return}u=l.value3;return}throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): "+[l.constructor.name])}for(;!o;)c=i(u);return c};return n}};var cA=function(t){return t instanceof Jt};var nn=function(t){return function(e){return function(r){var n=t,u=e,o=!1,c;function i(l,p,s){if(p instanceof Pr)return o=!0,s;if(p instanceof ce){if(p.value0 instanceof Li){n=l,u=p.value1,r=new _e(s,p.value0.value0,p.value0.value1,p.value0.value2);return}if(p.value0 instanceof ai){n=l,u=p.value1,r=new _e(p.value0.value0,p.value0.value1,p.value0.value2,s);return}if(p.value0 instanceof Wi){n=l,u=p.value1,r=new Be(s,p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof Fo){n=l,u=p.value1,r=new Be(p.value0.value0,p.value0.value1,p.value0.value2,s,p.value0.value3,p.value0.value4,p.value0.value5);return}if(p.value0 instanceof qi){n=l,u=p.value1,r=new Be(p.value0.value0,p.value0.value1,p.value0.value2,p.value0.value3,p.value0.value4,p.value0.value5,s);return}throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): "+[p.value0.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): "+[p.constructor.name,s.constructor.name])}for(;!o;)c=i(n,u,r);return c}}},Tl=function(t){return function(e){return function(r){var n=function(c){return function(i){var l=c,p=!1,s;function m(v,f){if(v instanceof Pr)return p=!0,new _e(f.value0,f.value1,f.value2,f.value3);if(v instanceof ce){if(v.value0 instanceof Li)return p=!0,nn(t)(v.value1)(new Be(f.value0,f.value1,f.value2,f.value3,v.value0.value0,v.value0.value1,v.value0.value2));if(v.value0 instanceof ai)return p=!0,nn(t)(v.value1)(new Be(v.value0.value0,v.value0.value1,v.value0.value2,f.value0,f.value1,f.value2,f.value3));if(v.value0 instanceof Wi){l=v.value1,i=new ap(new _e(f.value0,f.value1,f.value2,f.value3),v.value0.value0,v.value0.value1,new _e(v.value0.value2,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof Fo){l=v.value1,i=new ap(new _e(v.value0.value0,v.value0.value1,v.value0.value2,f.value0),f.value1,f.value2,new _e(f.value3,v.value0.value3,v.value0.value4,v.value0.value5));return}if(v.value0 instanceof qi){l=v.value1,i=new ap(new _e(v.value0.value0,v.value0.value1,v.value0.value2,v.value0.value3),v.value0.value4,v.value0.value5,new _e(f.value0,f.value1,f.value2,f.value3));return}throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): "+[v.value0.constructor.name,f.constructor.name])}throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): "+[v.constructor.name,f.constructor.name])}for(;!p;)s=m(l,i);return s}},u=Kt(t),o=function(c){return function(i){var l=c,p=!1,s;function m(v,f){if(f instanceof Jt)return p=!0,n(v)(new ap(Jt.value,e,r,Jt.value));if(f instanceof _e){var b=u(e)(f.value1);if(b instanceof Ae)return p=!0,nn(t)(v)(new _e(f.value0,e,r,f.value3));if(b instanceof Yt){l=new ce(new Li(f.value1,f.value2,f.value3),v),i=f.value0;return}l=new ce(new ai(f.value0,f.value1,f.value2),v),i=f.value3;return}if(f instanceof Be){var ct=u(e)(f.value1);if(ct instanceof Ae)return p=!0,nn(t)(v)(new Be(f.value0,e,r,f.value3,f.value4,f.value5,f.value6));var vt=u(e)(f.value4);if(vt instanceof Ae)return p=!0,nn(t)(v)(new Be(f.value0,f.value1,f.value2,f.value3,e,r,f.value6));if(ct instanceof Yt){l=new ce(new Wi(f.value1,f.value2,f.value3,f.value4,f.value5,f.value6),v),i=f.value0;return}if(ct instanceof ye&&vt instanceof Yt){l=new ce(new Fo(f.value0,f.value1,f.value2,f.value4,f.value5,f.value6),v),i=f.value3;return}l=new ce(new qi(f.value0,f.value1,f.value2,f.value3,f.value4,f.value5),v),i=f.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): "+[v.constructor.name,f.constructor.name])}for(;!p;)s=m(l,i);return s}};return o(Pr.value)}}},Kx=function(t){return function(e){var r=function(i){return function(l){var p=i,s=!1,m;function v(f,b){if(f instanceof Pr)return s=!0,b;if(f instanceof ce){if(f.value0 instanceof Li&&f.value0.value2 instanceof Jt&&b instanceof Jt)return s=!0,nn(t)(f.value1)(new _e(Jt.value,f.value0.value0,f.value0.value1,Jt.value));if(f.value0 instanceof ai&&f.value0.value0 instanceof Jt&&b instanceof Jt)return s=!0,nn(t)(f.value1)(new _e(Jt.value,f.value0.value1,f.value0.value2,Jt.value));if(f.value0 instanceof Li&&f.value0.value2 instanceof _e){p=f.value1,l=new Be(b,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3);return}if(f.value0 instanceof ai&&f.value0.value0 instanceof _e){p=f.value1,l=new Be(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,b);return}return f.value0 instanceof Li&&f.value0.value2 instanceof Be?(s=!0,nn(t)(f.value1)(new _e(new _e(b,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new _e(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6)))):f.value0 instanceof ai&&f.value0.value0 instanceof Be?(s=!0,nn(t)(f.value1)(new _e(new _e(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new _e(f.value0.value0.value6,f.value0.value1,f.value0.value2,b)))):f.value0 instanceof Wi&&f.value0.value2 instanceof Jt&&f.value0.value5 instanceof Jt&&b instanceof Jt?(s=!0,nn(t)(f.value1)(new Be(Jt.value,f.value0.value0,f.value0.value1,Jt.value,f.value0.value3,f.value0.value4,Jt.value))):f.value0 instanceof Fo&&f.value0.value0 instanceof Jt&&f.value0.value5 instanceof Jt&&b instanceof Jt?(s=!0,nn(t)(f.value1)(new Be(Jt.value,f.value0.value1,f.value0.value2,Jt.value,f.value0.value3,f.value0.value4,Jt.value))):f.value0 instanceof qi&&f.value0.value0 instanceof Jt&&f.value0.value3 instanceof Jt&&b instanceof Jt?(s=!0,nn(t)(f.value1)(new Be(Jt.value,f.value0.value1,f.value0.value2,Jt.value,f.value0.value4,f.value0.value5,Jt.value))):f.value0 instanceof Wi&&f.value0.value2 instanceof _e?(s=!0,nn(t)(f.value1)(new _e(new Be(b,f.value0.value0,f.value0.value1,f.value0.value2.value0,f.value0.value2.value1,f.value0.value2.value2,f.value0.value2.value3),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Fo&&f.value0.value0 instanceof _e?(s=!0,nn(t)(f.value1)(new _e(new Be(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3,f.value0.value1,f.value0.value2,b),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Fo&&f.value0.value5 instanceof _e?(s=!0,nn(t)(f.value1)(new _e(f.value0.value0,f.value0.value1,f.value0.value2,new Be(b,f.value0.value3,f.value0.value4,f.value0.value5.value0,f.value0.value5.value1,f.value0.value5.value2,f.value0.value5.value3)))):f.value0 instanceof qi&&f.value0.value3 instanceof _e?(s=!0,nn(t)(f.value1)(new _e(f.value0.value0,f.value0.value1,f.value0.value2,new Be(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3,f.value0.value4,f.value0.value5,b)))):f.value0 instanceof Wi&&f.value0.value2 instanceof Be?(s=!0,nn(t)(f.value1)(new Be(new _e(b,f.value0.value0,f.value0.value1,f.value0.value2.value0),f.value0.value2.value1,f.value0.value2.value2,new _e(f.value0.value2.value3,f.value0.value2.value4,f.value0.value2.value5,f.value0.value2.value6),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Fo&&f.value0.value0 instanceof Be?(s=!0,nn(t)(f.value1)(new Be(new _e(f.value0.value0.value0,f.value0.value0.value1,f.value0.value0.value2,f.value0.value0.value3),f.value0.value0.value4,f.value0.value0.value5,new _e(f.value0.value0.value6,f.value0.value1,f.value0.value2,b),f.value0.value3,f.value0.value4,f.value0.value5))):f.value0 instanceof Fo&&f.value0.value5 instanceof Be?(s=!0,nn(t)(f.value1)(new Be(f.value0.value0,f.value0.value1,f.value0.value2,new _e(b,f.value0.value3,f.value0.value4,f.value0.value5.value0),f.value0.value5.value1,f.value0.value5.value2,new _e(f.value0.value5.value3,f.value0.value5.value4,f.value0.value5.value5,f.value0.value5.value6)))):f.value0 instanceof qi&&f.value0.value3 instanceof Be?(s=!0,nn(t)(f.value1)(new Be(f.value0.value0,f.value0.value1,f.value0.value2,new _e(f.value0.value3.value0,f.value0.value3.value1,f.value0.value3.value2,f.value0.value3.value3),f.value0.value3.value4,f.value0.value3.value5,new _e(f.value0.value3.value6,f.value0.value4,f.value0.value5,b)))):(s=!0,pu("The impossible happened in partial function `up`."))}throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): "+[f.constructor.name])}for(;!s;)m=v(p,l);return m}},n=function(i){return function(l){var p=i,s=!1,m;function v(f,b){if(b instanceof _e&&b.value0 instanceof Jt&&b.value3 instanceof Jt)return s=!0,r(f)(Jt.value);if(b instanceof _e){p=new ce(new ai(b.value0,b.value1,b.value2),f),l=b.value3;return}if(b instanceof Be&&b.value0 instanceof Jt&&b.value3 instanceof Jt&&b.value6 instanceof Jt)return s=!0,r(new ce(new ai(Jt.value,b.value1,b.value2),f))(Jt.value);if(b instanceof Be){p=new ce(new qi(b.value0,b.value1,b.value2,b.value3,b.value4,b.value5),f),l=b.value6;return}return s=!0,pu("The impossible happened in partial function `removeMaxNode`.")}for(;!s;)m=v(p,l);return m}},u=function(i){var l=!1,p;function s(m){if(m instanceof _e&&m.value3 instanceof Jt)return l=!0,{key:m.value1,value:m.value2};if(m instanceof _e){i=m.value3;return}if(m instanceof Be&&m.value6 instanceof Jt)return l=!0,{key:m.value4,value:m.value5};if(m instanceof Be){i=m.value6;return}return l=!0,pu("The impossible happened in partial function `maxNode`.")}for(;!l;)p=s(i);return p},o=Kt(t),c=function(i){return function(l){var p=i,s=!1,m;function v(f,b){if(b instanceof Jt)return s=!0,z.value;if(b instanceof _e){var ct=o(e)(b.value1);if(b.value3 instanceof Jt&&ct instanceof Ae)return s=!0,new B(new rt(b.value2,r(f)(Jt.value)));if(ct instanceof Ae){var vt=u(b.value0);return s=!0,new B(new rt(b.value2,n(new ce(new Li(vt.key,vt.value,b.value3),f))(b.value0)))}if(ct instanceof Yt){p=new ce(new Li(b.value1,b.value2,b.value3),f),l=b.value0;return}p=new ce(new ai(b.value0,b.value1,b.value2),f),l=b.value3;return}if(b instanceof Be){var Qt=function(){return b.value0 instanceof Jt&&b.value3 instanceof Jt&&b.value6 instanceof Jt}(),ct=o(e)(b.value4),ee=o(e)(b.value1);if(Qt&&ee instanceof Ae)return s=!0,new B(new rt(b.value2,nn(t)(f)(new _e(Jt.value,b.value4,b.value5,Jt.value))));if(Qt&&ct instanceof Ae)return s=!0,new B(new rt(b.value5,nn(t)(f)(new _e(Jt.value,b.value1,b.value2,Jt.value))));if(ee instanceof Ae){var vt=u(b.value0);return s=!0,new B(new rt(b.value2,n(new ce(new Wi(vt.key,vt.value,b.value3,b.value4,b.value5,b.value6),f))(b.value0)))}if(ct instanceof Ae){var vt=u(b.value3);return s=!0,new B(new rt(b.value5,n(new ce(new Fo(b.value0,b.value1,b.value2,vt.key,vt.value,b.value6),f))(b.value3)))}if(ee instanceof Yt){p=new ce(new Wi(b.value1,b.value2,b.value3,b.value4,b.value5,b.value6),f),l=b.value0;return}if(ee instanceof ye&&ct instanceof Yt){p=new ce(new Fo(b.value0,b.value1,b.value2,b.value4,b.value5,b.value6),f),l=b.value3;return}p=new ce(new qi(b.value0,b.value1,b.value2,b.value3,b.value4,b.value5),f),l=b.value6;return}throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): "+[b.constructor.name])}for(;!s;)m=v(p,l);return m}};return c(Pr.value)}},Sa={foldr:function(t){return function(e){return function(r){if(r instanceof Jt)return e;if(r instanceof _e)return ir(Sa)(t)(t(r.value2)(ir(Sa)(t)(e)(r.value3)))(r.value0);if(r instanceof Be)return ir(Sa)(t)(t(r.value2)(ir(Sa)(t)(t(r.value5)(ir(Sa)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): "+[r.constructor.name])}}},foldl:function(t){return function(e){return function(r){if(r instanceof Jt)return e;if(r instanceof _e)return gr(Sa)(t)(t(gr(Sa)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof Be)return gr(Sa)(t)(t(gr(Sa)(t)(t(gr(Sa)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): "+[r.constructor.name])}}},foldMap:function(t){return function(e){return function(r){if(r instanceof Jt)return De(t);if(r instanceof _e)return St(t.Semigroup0())(Cn(Sa)(t)(e)(r.value0))(St(t.Semigroup0())(e(r.value2))(Cn(Sa)(t)(e)(r.value3)));if(r instanceof Be)return St(t.Semigroup0())(Cn(Sa)(t)(e)(r.value0))(St(t.Semigroup0())(e(r.value2))(St(t.Semigroup0())(Cn(Sa)(t)(e)(r.value3))(St(t.Semigroup0())(e(r.value5))(Cn(Sa)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): "+[r.constructor.name])}}}},oa={foldrWithIndex:function(t){return function(e){return function(r){if(r instanceof Jt)return e;if(r instanceof _e)return To(oa)(t)(t(r.value1)(r.value2)(To(oa)(t)(e)(r.value3)))(r.value0);if(r instanceof Be)return To(oa)(t)(t(r.value1)(r.value2)(To(oa)(t)(t(r.value4)(r.value5)(To(oa)(t)(e)(r.value6)))(r.value3)))(r.value0);throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): "+[r.constructor.name])}}},foldlWithIndex:function(t){return function(e){return function(r){if(r instanceof Jt)return e;if(r instanceof _e)return Ku(oa)(t)(t(r.value1)(Ku(oa)(t)(e)(r.value0))(r.value2))(r.value3);if(r instanceof Be)return Ku(oa)(t)(t(r.value4)(Ku(oa)(t)(t(r.value1)(Ku(oa)(t)(e)(r.value0))(r.value2))(r.value3))(r.value5))(r.value6);throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): "+[r.constructor.name])}}},foldMapWithIndex:function(t){return function(e){return function(r){if(r instanceof Jt)return De(t);if(r instanceof _e)return St(t.Semigroup0())(ti(oa)(t)(e)(r.value0))(St(t.Semigroup0())(e(r.value1)(r.value2))(ti(oa)(t)(e)(r.value3)));if(r instanceof Be)return St(t.Semigroup0())(ti(oa)(t)(e)(r.value0))(St(t.Semigroup0())(e(r.value1)(r.value2))(St(t.Semigroup0())(ti(oa)(t)(e)(r.value3))(St(t.Semigroup0())(e(r.value4)(r.value5))(ti(oa)(t)(e)(r.value6)))));throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): "+[r.constructor.name])}}},Foldable0:function(){return Sa}},lA=function(){return To(oa)(function(t){return function(e){return function(r){return new ce(t,r)}}})(Pr.value)}();var op=function(){return Jt.value}();var Om=function(t){return function(e){return function(r){return jr(r)(tn)(Kx(t)(e)(r))}}};var ip=function(t){return function(e){return function(r){return function(n){var u=e(Qx(t)(r)(n));if(u instanceof z)return Om(t)(r)(n);if(u instanceof B)return Tl(t)(r)(u.value0)(n);throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): "+[u.constructor.name])}}}};var Yx=function(t){return function(e){return function(r){return function(n){var u=function(o){return function(c){return function(i){return ip(t)(function(){var l=jr(i)(e(i));return function(p){return B.create(l(p))}}())(o)(c)}}};return Ku(oa)(u)(n)(r)}}}};var _A=function(t){return Yx(t)(S)};var Fl=function(t){return t.partitionMap};var Gi=function(t){return t.filterMap};var $l=function(t){return t.filter};var nF=function(t){return t},wl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Pl=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),aF=function(t){return t};var fp=Ca(),d=aF;var q=function(){return wl.create}();var ft=function(){return Pl.create}(),rr=function(){var t=_(ja)(_(C)(S(!0)));return function(e){return nF(t(e))}}(),Q=function(t){return t.attr};function DA(t){return function(){var e={};for(var r in t)hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}}var Tu={};function Nm(t){return t()}function dA(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(t[n]));return r}function bA(t,e){var r={};for(var n in t)hasOwnProperty.call(t,n)&&(r[n]=e(n)(t[n]));return r}function yA(t){return function(e){return function(r){return function(n){var u=r;function o(i){return function(l){return e(l)(i)(n[i])}}for(var c in n)hasOwnProperty.call(n,c)&&(u=t(u)(o(c)));return u}}}}function Ml(t){return function(e){var r=[];for(var n in e)hasOwnProperty.call(e,n)&&r.push(t(n)(e[n]));return r}}var iF=Object.keys||Ml(function(t){return function(){return t}});function Wm(t){return function(e){return function(r){return function(){return r[t]=e,r}}}}var qm=function(t){return function(e){return function(){return delete e[t],e}}};var Gm=Ml(function(t){return function(e){return e}});var dF=DA;var gA=function(t){return function(e){return Nm(function(){var n=dF(e)();return t(n)(),n})}};var EA=function(t){return function(e){return bA(e,t)}};var xu=function(t){return function(e){return gA(Wm(t)(e))}},cp={map:function(t){return function(e){return dA(e,t)}}},bF={mapWithIndex:EA,Functor0:function(){return cp}},Bm=function(){return _t};var lp=yA(zf),CA=function(t){return function(e){return lp(function(r){return function(n){return function(u){return St(t.Semigroup0())(r)(e(n)(u))}}})(De(t))}},bf={foldl:function(t){return lp(function(e){return function(r){return t(e)}})},foldr:function(t){return function(e){return function(r){return ir(wt)(t)(e)(Gm(r))}}},foldMap:function(t){return function(e){return CA(t)(S(e))}}},SA={foldlWithIndex:function(t){return lp(ht(t))},foldrWithIndex:function(t){return function(e){return function(r){return ir(wt)(Zf(t))(e)(Ml(rt.create)(r))}}},foldMapWithIndex:function(t){return CA(t)},Foldable0:function(){return bf}},yF={traverseWithIndex:function(t){return function(e){return function(r){return lp(function(n){return function(u){return function(o){return Vt(t.Apply0())(_(t.Apply0().Functor0())(ht(xu(u)))(n))(e(u)(o))}}})(L(t)(Tu))(r)}}},FunctorWithIndex0:function(){return bF},FoldableWithIndex1:function(){return SA},Traversable2:function(){return uc}},uc={traverse:function(t){var e=Ni(yF)(t);return function(r){return e(S(r))}},sequence:function(t){return Jn(uc)(t)(Y(Z))},Functor0:function(){return cp},Foldable1:function(){return bf}};var oc=function(t){return gA(qm(t))};var gF=function(){function t(c){this.fn=c}var e={},r=function(c,i){this.head=c,this.tail=i};function n(c){return new r(c,e)}function u(c){return function(i){return new r(c,i)}}function o(c){for(var i=[],l=c;l!==e;)i.push(l.head),l=l.tail;return i}return function(c){return function(i){return function(l){var p=function(m,v){return c(i(u)(l(m)))(v)},s=function(m,v,f){if(v===0)return m;var b=f[v-1];return new t(function(){var ct=s(p(b,m),v-1,f);return ct})};return function(m){for(var v=i(n)(l(m[m.length-1])),f=s(v,m.length-1,m);f instanceof t;)f=f.fn();return i(o)(f)}}}}}();var xA=function(t){return t};var wA=xA,Il=function(t){return t};var PA=function(t){return wA(Vy(t))};var ic=function(t){if(Gu(t)>0)return new B(wA(t));if(nr)return z.value;throw new Error("Failed pattern match at Data.Array.NonEmpty (line 157, column 1 - line 157, column 58): "+[t.constructor.name])};var MA=function(t){return function(e){return t(Il(e))}};var OA=MA(Gu);var IA=function(){return MA(Cm())};var RA=function(t){return function(e){return function(r){return(r|0)===r?t(r):e}}},Xe=function(t){return t};var Vm=function(t){return function(e){return Math.pow(t,e)|0}};var _p=isFinite;var Rl=Math.floor;var Ui=function(t){return function(e){return Math.pow(t,e)}},Nl=function(t){return function(e){return t%e}},pp=Math.round;var sp=Math.sin;var Hi=3.141592653589793;var fc=function(){return RA(B.create)(z.value)}(),LA=function(t){if(!_p(t))return 0;if(t>=Xe(Pn(cf)))return Pn(cf);if(t<=Xe(Mn(cf)))return Mn(cf);if(nr)return ga(0)(fc(t));throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): "+[t.constructor.name])},WA=function(t){return LA(pp(t))};var Ll=function(t){return LA(Rl(t))};var Jm=function(){var t=tp(Fm);return function(e){return ep(t(e))}}();var UF=function(t){return function(e){return function(r){return _A(t)(e)(r)}}};var jm=function(t){return lA(t)};var GA=function(t){return fA(t)(void 0)};var Xm=function(t){return{append:UF(t)}};var BA=function(t){return cA(t)},UA=function(t){return function(e){return function(r){return Tl(t)(e)(void 0)(r)}}};var HA={foldMap:function(t){return function(e){var r=Cn(Ba)(t)(e);return function(n){return r(jm(n))}}},foldl:function(t){return function(e){var r=gr(Ba)(t)(e);return function(n){return r(jm(n))}}},foldr:function(t){return function(e){var r=ir(Ba)(t)(e);return function(n){return r(jm(n))}}}};var Qm=op;var zA=function(t){return{mempty:Qm,Semigroup0:function(){return Xm(t)}}};var vp=function(t){return function(e){return function(r){return Om(t)(e)(r)}}};var o4=typeof Array.from=="function",i4=typeof Symbol<"u"&&Symbol!=null&&typeof Symbol.iterator<"u"&&typeof String.prototype[Symbol.iterator]=="function",f4=typeof String.prototype.fromCodePoint=="function",c4=typeof String.prototype.codePointAt=="function";function ql(t,e){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);for(var u in t)({}).hasOwnProperty.call(t,u)&&(r[u]=t[u]);return r}var tk=function(t){return function(){return function(){return function(e){return function(r){return function(n){return qu(Tr(t)(e))(r)(n)}}}}}};var ek=function(){return function(){return function(t){return function(e){return ql(t,e)}}}},Gl=function(t){return function(){return function(){return function(e){return function(r){return function(n){return qu(Tr(t)(e))(r)(n)}}}}}},ui=function(t){return function(){return function(e){return function(r){return Xa(Tr(t)(e))(r)}}}};var a={proof:function(t){return t},Coercible0:function(){}},Af=function(t){return t.proof};var su=void 0;var gp=function(t){return t.toInt},rk=function(t){return function(e){return gp(t)(su)}};var Qa={toInt:function(t){return 8}},nk={Nat0:function(){return Qa}},wo={toInt:function(t){return 7}},ak={Nat0:function(){return wo}},Po={toInt:function(t){return 6}},uk={Nat0:function(){return Po}},Ta={toInt:function(t){return 5}},Ep={Nat0:function(){return Ta}},Rn={toInt:function(t){return 4}},Qn={Nat0:function(){return Rn}},Nn={toInt:function(t){return 3}},xa={Nat0:function(){return Nn}},Ln={toInt:function(t){return 2}},Fa={Nat0:function(){return Ln}},Wn={toInt:function(t){return 1}},ia={Nat0:function(){return Wn}},yr={toInt:function(t){return 0}};var de=function(t){return function(){return function(e){return function(){return function(r){return{Nat0:e.Nat1,Pos1:function(){return t}}}}}}};var to={Nat0:function(){return wo},Nat1:function(){return Qa}};var eo={Nat0:function(){return Po},Nat1:function(){return Qa}};var ro={Nat0:function(){return Ta},Nat1:function(){return Qa}};var no={Nat0:function(){return Rn},Nat1:function(){return Qa}};var fa={Nat0:function(){return Rn},Nat1:function(){return Ta}};var ao={Nat0:function(){return Nn},Nat1:function(){return Qa}};var ca={Nat0:function(){return Nn},Nat1:function(){return Ta}};var uo={Nat0:function(){return Ln},Nat1:function(){return Qa}};var la={Nat0:function(){return Ln},Nat1:function(){return Ta}};var oo={Nat0:function(){return Wn},Nat1:function(){return Qa}};var _a={Nat0:function(){return Wn},Nat1:function(){return Ta}};var io={Nat0:function(){return yr},Nat1:function(){return Qa}};var pa={Nat0:function(){return yr},Nat1:function(){return Ta}};var ok={Nat0:function(){return yr},Nat1:function(){return Qa}};var ov={Nat0:function(){return yr},Nat1:function(){return wo}};var iv={Nat0:function(){return yr},Nat1:function(){return Po}};var Bl={Nat0:function(){return yr},Nat1:function(){return Ta}};var Ua={Nat0:function(){return yr},Nat1:function(){return Rn}};var Gr={Nat0:function(){return yr},Nat1:function(){return Nn}};var Mr={Nat0:function(){return yr},Nat1:function(){return Ln}};var xr={Nat0:function(){return yr},Nat1:function(){return Wn}},sa={Nat0:function(){return yr},Nat1:function(){return yr}};var oi=function(t){return t.state};function Mo(t){return new Error(t)}function Ha(t){return function(){throw t}}function Cp(t){return function(e){return function(){try{return e()}catch(r){return r instanceof Error||Object.prototype.toString.call(r)==="[object Error]"?t(r)():t(new Error(r.toString()))()}}}}var co=function(t){return t.throwError};var R$={throwError:Ha,Monad0:function(){return pf}};var cv={catchError:ht(Cp),MonadThrow0:function(){return R$}};var ii=function(t){return t.catchError};var Hl=function(t){return function(e){return ii(t)(_(t.MonadThrow0().Monad0().Bind1().Apply0().Functor0())(Ot.create)(e))(function(){var r=L(t.MonadThrow0().Monad0().Applicative0());return function(n){return r(Mt.create(n))}}())}};var fr={liftEffect:Y(Z),Monad0:function(){return pf}},vr=function(t){return t.liftEffect};var _v=function(t){return{map:function(e){return function(r){return function(n){return _(t)(function(u){return new rt(e(u.value0),u.value1)})(r(n))}}}}};var pv=function(t){return{Applicative0:function(){return mv(t)},Bind1:function(){return sv(t)}}},sv=function(t){return{bind:function(e){return function(r){return function(n){return nt(t.Bind1())(e(n))(function(u){var o=r(u.value0);return o(u.value1)})}}},Apply0:function(){return Fp(t)}}},Fp=function(t){return{apply:Bu(pv(t)),Functor0:function(){return _v(t.Bind1().Apply0().Functor0())}}},mv=function(t){return{pure:function(e){return function(r){return L(t.Applicative0())(new rt(e,r))}},Apply0:function(){return Fp(t)}}};var pk=function(t){return{state:function(e){var r=L(t.Applicative0());return function(n){return r(e(n))}},Monad0:function(){return pv(t)}}};var mk=function(t){return function(e){var r=t(e);return r.value0}};var Ka=Math.random;var Xl=function(t){return function(e){return function(){var n=Ka(),u=(Xe(e)-Xe(t)+1)*n+Xe(t);return Ll(u)}}};var vk=function(t){return t};var z$=1,$p=2147483647,V$=function(){return $p-1|0}(),wp=function(t){var e=function(r){return function(n){return function(u){var o=n-r|0,c=iu(Co)(u)(o),i=c<r;return i?c+n|0:c}}};return e(z$)(V$)(t)};var J$=0,j$=48271,Dk=function(t){return function(e){return Vn()(fc(Nl(Xe(j$)*Xe(e)+Xe(t))(Xe($p))))}},dk=Dk(J$);var X$=function(t){return t};var yk=function(){var t=function(e){return new rt(vk(e.newSeed),function(){var r={};for(var n in e)({}).hasOwnProperty.call(e,n)&&(r[n]=e[n]);return r.newSeed=dk(e.newSeed),r}())};return oi(pk(zu))(t)}();var lo=_v(Eo),Ak=_(lo)(function(t){return Xe(t)/Xe($p)})(yk);var Pp=function(t){return mk(X$(t))};var Cf=sv(zu);var Sf=Fp(zu),bk=function(t){return function(e){var r=Xe(e),n=Xe(t),u=function(i){return n+Nl(i)(r-n+1)},o=_(lo)(Xe)(yk),c=Vt(Sf)(_(lo)(Ve(Aa))(o))(_(lo)(wn(Aa)(2))(o));return _(lo)(function(i){return Ll(u(i))})(c)}},kk=function(t){return function(e){var r=t<=e;return r?bk(t)(e):bk(e)(t)}};var Ql=mv(zu);var Dv=function(t){return nt(Cf)(kk(0)(OA(t)-1|0))(function(e){return L(Ql)(IA()(t)(e))})};var dv=function(t){return t.arbitrary};var gk={arbitrary:Ak};var Ck=Yo;var Oo=function(t){return t};var hf=function(t){return function(){return function(e){return function(r){return e[gp(t)(r)]}}}};var yv=mf,Av=Pe;var kv=wt;var Op=function(t){return function(e){var r=rk(t)(D.value),n=function(){return r===0?[]:sn(0)(r-1|0)}();return _(Pe)(e)(n)}};var $a=[];var ke=function(t){return function(e){return function(r){return ac(e)(r)}}},Sk=function(t){return ke(de(ia)()(sa)()(xr))(t)($a)};var Ip=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),hk=function(){function t(){}return t.value=new t,t}(),Rp=function(){function t(){}return t.value=new t,t}();var Np=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Lp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Wp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),qp=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var vu=function(){function t(){this.head=null,this.last=null,this.size=0}function e(s,m){this.queue=s,this.value=m,this.next=null,this.prev=null}function r(s){this.draining=!1,this.error=null,this.value=s,this.takes=new t,this.reads=new t,this.puts=new t}var n={};function u(s){try{s()}catch(m){setTimeout(function(){throw m},0)}}function o(s,m){var v=new e(s,m);switch(s.size){case 0:s.head=v;break;case 1:v.prev=s.head,s.head.next=v,s.last=v;break;default:v.prev=s.last,s.last.next=v,s.last=v}return s.size++,v}function c(s){var m;switch(s.size){case 0:return null;case 1:m=s.head,s.head=null;break;case 2:m=s.last,s.head.next=null,s.last=null;break;default:m=s.last,s.last=m.prev,s.last.next=null}return m.prev=null,m.queue=null,s.size--,m.value}function i(s){var m;switch(s.size){case 0:return null;case 1:m=s.head,s.head=null;break;case 2:m=s.head,s.last.prev=null,s.head=s.last,s.last=null;break;default:m=s.head,s.head=m.next,s.head.prev=null}return m.next=null,m.queue=null,s.size--,m.value}function l(s){if(s.queue!==null){if(s.queue.last===s){c(s.queue);return}if(s.queue.head===s){i(s.queue);return}s.prev&&(s.prev.next=s.next),s.next&&(s.next.prev=s.prev),s.queue.size--,s.queue=null,s.value=null,s.next=null,s.prev=null}}function p(s,m){if(!m.draining){var v=m.puts,f=m.takes,b=m.reads,ct,vt,Qt,ee,xe;for(m.draining=!0;;){if(ct=null,vt=null,Qt=null,ee=m.value,xe=b.size,m.error!==null){for(ee=s.left(m.error);ct=i(v);)u(ct.cb(ee));for(;vt=i(b);)u(vt(ee));for(;Qt=i(f);)u(Qt(ee));break}if(ee===n&&(ct=i(v))&&(m.value=ee=ct.value),ee!==n){for(Qt=i(f);xe--&&(vt=i(b));)u(vt(s.right(ee)));Qt!==null&&(m.value=n,u(Qt(s.right(ee))))}if(ct!==null&&u(ct.cb(s.right(void 0))),m.value===n&&v.size===0||m.value!==n&&f.size===0)break}m.draining=!1}}return r.EMPTY=n,r.putLast=o,r.takeLast=c,r.takeHead=i,r.deleteCell=l,r.drainVar=p,r}();function Du(){return new vu(vu.EMPTY)}function xk(t,e,r){return function(){var n=vu.putLast(e.takes,r);return vu.drainVar(t,e),function(){vu.deleteCell(n)}}}function Fk(t,e,r){return function(){var n=vu.putLast(e.reads,r);return vu.drainVar(t,e),function(){vu.deleteCell(n)}}}function $k(t,e,r){return function(){return r.value===vu.EMPTY&&r.error===null?(r.value=e,vu.drainVar(t,r),!0):!1}}function wk(t,e){return function(){var r=e.value;return r===vu.EMPTY?t.nothing:(e.value=vu.EMPTY,vu.drainVar(t,e),t.just(r))}}var Y$=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Z$=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),tw=function(){function t(){}return t.value=new t,t}();var Gp=function(){return{left:Mt.create,right:Ot.create,nothing:z.value,just:B.create,killed:Y$.create,filled:Z$.create,empty:tw.value}}();var Pk=function(t){return function(e){return Fk(Gp,t,e)}};var ci=function(t){return function(e){return xk(Gp,t,e)}},wu=function(t){return function(e){return $k(Gp,t,e)}};var Mk=function(t){return wk(Gp,t)};var Tf=function(t){return t()};var _o=function(t){return t.sampleOn};var Dr=function(t){return t.keepLatest},Pu=function(t){return t.fold};var Yl=function(t){return function(e){return function(r){return function(n){return Gi(t.Filterable1())(tn)(Pu(t)(function(u){return function(o){return _(Qo)(L(go))(e(u)(o.value0))}})(r)(new rt(n,z.value)))}}}},Bp=function(t){return function(e){var r=function(n){return function(u){if(u instanceof z)return new B({now:n,last:z.value});if(u instanceof B)return new B({now:n,last:new B(u.value0.now)});throw new Error("Failed pattern match at FRP.Event.Class (line 56, column 3 - line 56, column 50): "+[n.constructor.name,u.constructor.name])}};return Gi(t.Filterable1())(Y(Z))(Pu(t)(r)(e)(z.value))}},Zl=function(t){return t.fix};var Tn=function(t){return function(e){return function(r){return P(t.Plus0().Alt0())(_o(t)(e)(r))(_o(t)(r)(_(t.Filterable1().Functor1())(zf)(e)))}}},K=function(t){return t.bang};function gv(t){return function(e){return t===e}}var Ev=gv;var nw=function(t){return t},gt=function(t){return function(e){return t(e)}},aw=function(t){return function(e){return function(r){return function(){var u=ge(z.value)(),o=t(function(i){return je(new B(i))(u)})(),c=e(function(i){return nt(Ge)(Me(u))(mr(tt)(ur)(function(l){return r(i(l))}))})();return H(J)(o)(c)}}}},It=nw,uw=function(t){return function(e){return function(){var n=ge(z.value)(),u=t(function(o){return function(){nt(Ge)(Me(n))(V_(tt)(ur))();var i=gt(o)(e)();return je(new B(i))(n)()}})();return function(){return nt(Ge)(Me(n))(V_(tt)(ur))(),u()}}}},A={map:function(t){return function(e){return function(r){return e(function(n){return r(t(n))})}}}},ow=function(t){return function(e){return function(r){return function(n){return function(){var o=ge(r)();return e(function(c){return nt(Ge)(Oi(t(c))(o))(n)})()}}}}},t_=function(t){return function(e){return function(r){return e(function(n){var u=t(n);if(u instanceof B)return r(u.value0);if(u instanceof z)return L(tt)(void 0);throw new Error("Failed pattern match at FRP.Event (line 108, column 13 - line 110, column 27): "+[u.constructor.name])})}}},Cv=function(t){return t_(function(e){var r=t(e);if(r)return new B(e);if(!r)return z.value;throw new Error("Failed pattern match at FRP.Event (line 66, column 13 - line 68, column 25): "+[r.constructor.name])})},e_=function(){var e=ge([])();return{event:function(r){return function(){return Oi(function(u){return St(uu)(u)([r])})(e)(),function(){return Oi(hm(Ev)(r))(e)(),void 0}}},push:function(r){return nt(Ge)(Me(e))(mr(tt)(wt)(function(n){return n(r)}))}}},iw=function(t){var e=Tf(e_),r=t(e.event);return function(n){return function(){var o=gt(r.input)(e.push)(),c=gt(r.output)(n)();return H(J)(o)(c)}}},ji=function(t){return function(e){return It(function(r){return function(){var u=e_();return r(e(u.event))(),gt(t)(u.push)()}})}},Ik={compact:t_(Y(Z)),separate:function(t){return{left:t_(function(e){if(e instanceof Mt)return new B(e.value0);if(e instanceof Ot)return z.value;throw new Error("Failed pattern match at FRP.Event (line 49, column 13 - line 51, column 33): "+[e.constructor.name])})(t),right:t_(function(e){if(e instanceof Ot)return new B(e.value0);if(e instanceof Mt)return z.value;throw new Error("Failed pattern match at FRP.Event (line 56, column 13 - line 58, column 32): "+[e.constructor.name])})(t)}}},du={filter:Cv,filterMap:t_,partition:function(t){return function(e){return{yes:Cv(t)(e),no:Cv(function(){var r=lu(Wa);return function(n){return r(t(n))}}())(e)}}},partitionMap:function(t){return function(e){return{left:Gi(du)(function(){var r=Na(B.create)(S(z.value));return function(n){return r(t(n))}}())(e),right:Gi(du)(function(r){return W_(t(r))})(e)}}},Compactable0:function(){return Ik},Functor1:function(){return A}},an=function(t){return It(function(e){return function(){var n=e_();return e(t(n.push)(n.event))(),L(tt)(void 0)}})},Tt=function(t){return function(e){return function(){return e(t)(),L(tt)(void 0)}}},R={alt:function(t){return function(e){return function(r){return function(){var u=t(r)(),o=e(r)();return H(J)(u)(o)}}}},Functor0:function(){return A}},g={empty:function(t){return L(tt)(L(tt)(void 0))},Alt0:function(){return R}},E={fold:ow,keepLatest:uw,sampleOn:aw,fix:iw,bang:Tt,Plus0:function(){return g},Filterable1:function(){return du}};var Rk=function(){function t(){}return t.value=new t,t}(),Sv=function(){function t(){}return t.value=new t,t}(),fw=function(){function t(){}return t.value=new t,t}(),pc=function(t){return function(e){var r=function(n){return n({parent:t,scope:"trivial",raiseId:De(Pi(So(Xo)))})(e)};return function(n){if(n instanceof Lp)return Lr(wt)(g)(r)(n.value0);if(n instanceof Wp)return Dr(E)(_(A)(pc(t)(e))(n.value0));if(n instanceof qp)return r(n.value0);if(n instanceof Np)return It(function(u){return function(){var c=ge(Tu)(),i=gt(n.value0)(function(l){return function(){var s=e.ids(),m=ge(L(tt)(void 0))(),v=e.ids(),f=ge(L(tt)(void 0))(),b=ge(z.value)(),ct=ge(L(tt)(void 0))(),vt=e.ids(),Qt=ge(Rk.value)(),ee=gt(l)(function(xe){return function(){var it=Me(Qt)();if(xe instanceof hk&&it instanceof Sv)return nt(Ge)(Me(b))(mr(tt)(ur)(function(Rr){return u(e.sendToTop(function(da){return{id:da}}(Rr)))}))();if(xe instanceof Rp&&it instanceof Sv){je(fw.value)(Qt)();var oe=H(J)(H(J)(H(J)(H(J)(nt(Ge)(Me(b))(mr(tt)(ur)(function(Rr){return u(e.disconnectElement({id:Rr,parent:t,scope:vt}))})))(ra(Ge)(Me(m))))(ra(Ge)(Me(f))))(na(oc(s))(c)))(na(oc(v))(c));return H(J)(je(oe)(ct))(oe)()}if(xe instanceof Ip&&it instanceof Rk){je(Sv.value)(Qt)();var be=Du(),qt=gt(xe.value0({parent:t,scope:vt,raiseId:function(Rr){return Le(C)(wu(Rr)(be))}})(e))(u)(),kr=ci(be)(function(Rr){if(Rr instanceof Ot)return function(){return je(new B(Rr.value0))(b)(),na(xu(v)(qt))(c)(),je(qt)(f)()};if(Rr instanceof Mt)return Ha(Rr.value0);throw new Error("Failed pattern match at Deku.Internal (line 85, column 48 - line 90, column 49): "+[Rr.constructor.name])})();return kr()}return void 0}})();return je(ee)(m)(),na(xu(s)(ee))(c)(),ra(Ge)(Me(ct))()}})();return function(){return nt(Ge)(Me(c))(Ko(bf)(So(Xo)))(),i()}}});throw new Error("Failed pattern match at Deku.Internal (line 28, column 61 - line 102, column 22): "+[n.constructor.name])}}};var cw=function(t){return function(e){return{plant:function(r){return new Np(Af(a)(Ca()(r)))}}}},N=function(t){return function(e){return{plant:function(r){return new qp(Af(e)(Ca()(r)))}}}},$e=function(t){return function(e){return{plant:function(r){return new Lp(Af(a)(Ca()(r)))}}}},lw=function(t){return t.makeText},_w=function(t){return function(e){return function(r){return _(A)(function(n){return t.setText(function(u){return{id:e,text:u}}(n))})(r)}}},pw=function(t){return function(e){return function(r){return _(A)(function(n){return function(u){if(u.value instanceof wl)return t.setProp({id:e,key:u.key,value:u.value.value0});if(u.value instanceof Pl)return t.setCb({id:e,key:u.key,value:u.value.value0});throw new Error("Failed pattern match at Deku.Control (line 72, column 24 - line 74, column 41): "+[u.value.constructor.name])}(fp(n))})(r)}}},sw=function(t){return t.makeElement},un=function(t){var e=function(r){return function(n){return It(function(u){return function(){var c=n.ids();return r.raiseId(c)(),_(C)(H(J)(u(n.deleteFromCache({id:c}))))(gt(tr(wt)(g)([Tt(lw(n)({id:c,parent:r.parent,scope:r.scope})),_w(n)(c)(t)]))(u))()}})}};return e},cr=function(t){return un(Tt(t))},F=function(t){return t.plant},Lk=function(t){return function(e){var r=function(n){var u=function(o){return function(c){return new rt(c+1|0,new rt(o,c))}};return Yl(E)(u)(n)(0)};return F(cw(a)(a))(Dr(E)(ji(r(e))(function(n){return _(A)(function(u){return P(R)(Tt(new Ip(t(u.value0))))(_(A)(S(Rp.value))($l(du)(function(){var o=fe(Fi)(u.value1+1|0);return function(c){return o(tn(c))}}())(n)))})(n)})))}},V=function(t){return function(e){return function(r){var n=function(u){return function(o){return It(function(c){return function(){var l=o.ids();return u.raiseId(l)(),_(C)(H(J)(c(o.deleteFromCache({id:l}))))(gt(P(R)(tr(wt)(g)([Tt(sw(o)({id:l,parent:u.parent,scope:u.scope,tag:t})),pw(o)(l)(e)]))(pc(l)(o)(r)))(c))()}})}};return n}}},hv=function(t){return function(e){return _t}};var Wk=function(t){return function(e){return{plant:function(r){return hv(t)(e)(r)}}}},et=function(t){return function(e){return{plant:function(r){return new Wp(_(A)(hv(t)(e))(r))}}}},mw=function(t){return function(e){return function(r){return It(function(n){return function(){var o=r.ids();return gt(P(R)(Tt(r.makeRoot({id:o,root:t})))(pc(o)(r)(hv(a)(a)(e))))(n)()}})}}};var qk=function(t){return function(e){return mw(t)(F($e(a)(a))(e))}},ae=function(){var t=function(e){return function(r){return It(function(n){return function(){var o=r.ids();return e.raiseId(o)(),_(C)(H(J)(n(r.deleteFromCache({id:o}))))(gt(Tt(r.makeNoop({id:o,parent:e.parent,scope:e.scope})))(n))()}})}};return t}();var sr=function(){function t(){}return t.value=new t,t}();var dr={attr:function(t){return function(e){return d({key:"click",value:ft(e)})}}};var Ht=function(){function t(){}return t.value=new t,t}();var Up={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var Gk={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var pt={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var Bk={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}},sc={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var Tv={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var Uk={attr:function(t){return function(e){return d({key:"style",value:q(e)})}}};var xv=function(t){return function(e){return function(r){return V("a")(e)(F(t)(r))}}};var Fe=function(t){return function(e){return function(r){return V("div")(e)(F(t)(r))}}},Ue=function(t){return Fe(t)(T(g))};var vc=function(t){return function(e){return function(r){return V("span")(e)(F(t)(r))}}},Fv=function(t){return vc(t)(T(g))};var Dw=function(t){for(var e="",r="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",n=r.length,u=0;u<t;u++)e+=r.charAt(Math.floor(Math.random()*n));return e},$v=function(t){return function(e){return function(r){return function(){e!=="@portal@"&&r.units[e].main.appendChild(r.units[t].main)}}}};var zk=function(t){return function(e){return function(){var r=t.id;e.scopes[t.scope]||(e.scopes[t.scope]=[]),e.scopes[t.scope].push(r),e.units[r]={listeners:{},parent:t.parent,scope:t.scope,main:document.createElement(t.tag)},t.parent===e.terminus&&e.terminalPtrs.push(t.id),$v(r)(t.parent)(e)()}}};function Vk(t){return function(e){return function(){var r=t.id;e.scopes[t.scope]||(e.scopes[t.scope]=[]),e.scopes[t.scope].push(r),e.units[r]={main:document.createTextNode(""),parent:t.parent,scope:t.scope},$v(r)(t.parent)(e)()}}}function wv(){return{units:{},scopes:{},unqidfr:Dw(10)}}function Jk(t){return function(e){return function(){var r=t.id,n=t.value;e.units[r].main.tagName==="INPUT"&&t.key==="value"?e.units[r].main.value=n:e.units[r].main.tagName==="INPUT"&&t.key==="checked"?e.units[r].main.checked=n==="true":e.units[r].main.setAttribute(t.key,n)}}}function jk(t){return function(e){return function(){var r=t.id,n=t.value;if(t.key==="@canvas-hack@"){let o=e.units[r].main.getContext("2d");n(o)()}else{e.units[r].listeners[t.key]&&e.units[r].main.removeEventListener(t.key,e.units[r].listeners[t.key]);var u=o=>n(o)();e.units[r].main.addEventListener(t.key,u),e.units[r].listeners[t.key]=u}}}}function Xk(t){return function(e){return function(){var r=t.id;e.units[r].main.nodeValue=t.text}}}var Qk=function(t){return function(e){return function(){var r=t.id,n=t.html,u=t.verb,o=t.cache,c=t.parent,i=t.scope;t.parent===e.terminus&&e.terminalPtrs.push(t.id);for(var l=Object.entries(o),p=0;p<l.length;p++){var s=l[p][0];l[p][1]===!0?n=n.replace(u+s+u,'data-deku-attr-internal="'+s+'"'):n=n.replace(u+s+u,'<span style="display:contents;" data-deku-elt-internal="'+s+'"></span>')}var m=document.createElement("div");m.innerHTML=n.trim(),e.scopes[t.dkScope]||(e.scopes[t.dkScope]=[]),e.scopes[t.dkScope].push(r),e.units[r]={listeners:{},scope:t.dkScope,parent:c,main:m.firstChild},m.querySelectorAll("[data-deku-attr-internal]").forEach(function(v){var f=v.getAttribute("data-deku-attr-internal");e.units[f+i]={listeners:{},main:v,scope:t.dkScope},e.scopes[t.dkScope].push(f+i)}),m.querySelectorAll("[data-deku-elt-internal]").forEach(function(v){var f=v.getAttribute("data-deku-elt-internal");e.units[f+i]={listeners:{},main:v,scope:t.dkScope},e.scopes[t.dkScope].push(f+i)}),$v(r)(c)(e)()}}};function Kk(t){return function(e){return function(){var r=t.id;e.units[r]={main:t.root}}}}function Yk(t){return function(e){return function(){var r=t.id;e.units[r]={noop:!0}}}}function Zk(t){return function(e){return function(){var r=t.id,n=t.parent;e.units[r].containingScope=t.scope,e.units[n].main.prepend(e.units[r].main)}}}function tg(t){return function(e){return function(){var r=t.id;e.units[r].noop||e.units[r].containingScope&&e.units[r].containingScope!==t.scope||e.units[r].main.remove()}}}function eg(t){return function(e){return function(){delete e.units[t.id]}}}function rg(t){return function(e){return function(){var r=t.id;e.units[r].main.parentNode.prepend(e.units[r].main)}}}var ng={ids:_(C)(jt(Xf))(Ka),makeElement:zk,makeRoot:Kk,makeText:Vk,makePursx:Qk,setProp:Jk,setCb:jk,setText:Xk,sendToTop:rg,makeNoop:Yk,deleteFromCache:eg,giveNewParent:Zk,disconnectElement:tg};var za={dimap:function(t){return function(e){return function(r){return function(n){return e(r(t(n)))}}}}},li=function(t){return t.dimap},po=function(t){return function(e){return li(t)(e)(Y(Z))}};var Xi=function(t){return t.reflectType};var X={pursxToElement:function(t){return function(e){return function(r){return{cache:Tu,element:function(n){return function(u){return T(g)}}}}}}},Pv=function(t){return t.pursxToElement},on=function(){return function(t){return function(e){return function(r){return{pursxToElement:function(n){return function(u){return function(o){var c=Pv(t)(n)(D.value)(o);return{cache:xu(Xi(e)(D.value))(!0)(c.cache),element:function(i){return function(l){return P(R)(_(A)(po(za)(fp)(function(p){if(p.value instanceof wl)return l.setProp({id:Xi(e)(D.value)+n,key:p.key,value:p.value.value0});if(p.value instanceof Pl)return l.setCb({id:Xi(e)(D.value)+n,key:p.key,value:p.value.value0});throw new Error("Failed pattern match at Deku.Pursx (line 3852, column 38 - line 3862, column 24): "+[p.value.constructor.name])}))(ui(r)()(D.value)(o)))(c.element(i)(l))}}}}}}}}}}},M=function(){return function(t){return function(e){return function(r){return{pursxToElement:function(n){return function(u){return function(o){var c=ui(r)()(D.value)(o),i=Pv(t)(n)(D.value)(o);return{cache:xu(Xi(e)(D.value))(!1)(i.cache),element:function(l){return function(p){return P(R)(pc(Xi(e)(D.value)+n)(p)(c))(i.element(l)(p))}}}}}}}}}}};var O=function(t){return function(e){return F(t)(e)}},le=function(t){return function(e){return function(){return function(){return function(r){return function(n){return function(u){return function(o){var c=function(i){return function(l){return It(function(p){return function(){var m=l.ids(),v=l.ids();i.raiseId(m)();var f=Pv(r)(v)(D.value)(o);return _(C)(H(J)(p(l.deleteFromCache({id:m}))))(gt(P(R)(Tt(l.makePursx({id:m,parent:i.parent,cache:f.cache,scope:v,dkScope:i.scope,html:Xi(t)(u),verb:Xi(e)(n)})))(f.element(i)(l)))(p))()}})}};return c}}}}}}}},$t=function(t){return function(){return function(){return function(e){return le(t)({reflectType:function(){return"~"}})()()(e)(D.value)}}}};function ag(t){var e={};for(var r in t)({}).hasOwnProperty.call(t,r)&&(e[r]=t[r]);return e}function ug(t){return function(e){return function(r){return r[t]=e,r}}}var Mv=zo;var Ov=function(){return function(){return function(t){return function(e){return function(r){return function(n){return ug(Tr(t)(e))(r)(n)}}}}}};var Iv=Z,og=function(t){return function(e){return t(ag(e))}},ig=ht(og)({});var bt=function(){return function(){return{defaults:ht(ek()())}}},gw=function(t){return t.defaults},yt={convertRecordOptions:function(t){return function(e){return function(r){return Y(Iv)}}}},cg=function(t){return t.convertRecordOptions},ma=function(t){return t.convertOptionsWithDefaults},At=function(){return function(t){return{convertOptions:function(e){return function(r){return ig(cg(t)(e)(D.value)(r))}}}}},Ew=function(t){return t.convertOptions},kt=function(t){return function(e){return{convertOptionsWithDefaults:function(r){return function(n){var u=gw(e)(n),o=Ew(t)(r);return function(c){return u(o(c))}}}}}},Cw=function(t){return t.convertOption},U=function(t){return function(e){return function(){return function(){return function(){return function(r){return{convertRecordOptions:function(n){return function(u){return function(o){return Ao(Mv)(Ov()()(r)(D.value)(Cw(e)(n)(D.value)(ui(r)()(D.value)(o))))(cg(t)(n)(D.value)(o))}}}}}}}}}};var hw=function(){return function(){return function(){return function(t){return function(e){return function(r){return R_(r.type)(t)?Xa(r.type)(t)(r.value):e(r)}}}}}};var He=function(){return function(t){return function(e){return function(r){return{type:Tr(t)(e),value:r}}}}};var Tw=function(t){return pu("Data.Variant: pattern match failure ["+(t.type+"]"))},Ar=function(){return function(){return function(){return function(t){return hw()()()(t)(Tw)}}}};var Ww=function(t){return t};var r_=function(){return He()({reflectSymbol:function(){return"nothing"}})(D.value)(void 0)}();var fn=function(){var t=He()({reflectSymbol:function(){return"just"}})(D.value);return function(e){return Ww(t(e))}}();var Dc={foldl:function(t){return function(e){return function(r){return Ar()()()({just:function(n){return t(e)(n)},nothing:function(n){return e}})(r)}}},foldr:function(t){return function(e){return function(r){return Ar()()()({just:function(n){return t(n)(e)},nothing:function(n){return e}})(r)}}},foldMap:function(t){return J_(Dc)(t)}};var vn={first:function(t){return function(e){return new rt(t(e.value0),e.value1)}},second:_(Qo),Profunctor0:function(){return za}},qn=function(t){return t.second},Hp=function(t){return t.first};var Yw=function(t){return function(e){return function(r){return function(n){return li(r)(t)(e)(n)}}}};var bg=function(){return function(){return function(t){return Yw(Ca())(Ca())(t)}}};var yg=function(){return function(){return function(t){return bg()()(t)}}};var eP=function(t){return function(e){return function(r){return li(e.Profunctor0())(t)(function(n){return n.value1(n.value0)})(Hp(e)(r))}}},Ag=function(t){return function(e){return function(r){return eP(function(n){return new rt(t(n),function(u){return e(n)(u)})})(r)}}};var kg=function(t){return function(){return function(){return function(e){return function(r){return Ag(ui(t)()(e))(ht(tk(t)()()(e)))(r)}}}}};var gg=function(t){return t};var oP=JSON.parse;var iP=JSON.stringify;var zp=function(t){return t};var Vp=function(t){return t};var Jp=function(t){return function(e){return t(e)}},n_=function(t){return{map:function(e){return Jp(_(t)(_(_f)(e)))}}};var Wv=function(t){return{Applicative0:function(){return a_(t)},Bind1:function(){return qv(t)}}},qv=function(t){return{bind:function(e){return function(r){return nt(t.Bind1())(e)(Na(function(){var n=L(t.Applicative0());return function(u){return n(Mt.create(u))}}())(function(n){var u=r(n);return u}))}},Apply0:function(){return Cg(t)}}},Cg=function(t){return{apply:Bu(Wv(t)),Functor0:function(){return n_(t.Bind1().Apply0().Functor0())}}},a_=function(t){return{pure:function(){var e=L(t.Applicative0());return function(r){return zp(e(Ot.create(r)))}}(),Apply0:function(){return Cg(t)}}};var Sg=function(t){return{throwError:function(){var e=L(t.Applicative0());return function(r){return zp(e(Mt.create(r)))}}(),Monad0:function(){return Wv(t)}}};var Gv=function(t){return function(e){return{alt:function(r){return function(n){return nt(e.Bind1())(r)(function(u){if(u instanceof Ot)return L(e.Applicative0())(new Ot(u.value0));if(u instanceof Mt)return nt(e.Bind1())(n)(function(o){if(o instanceof Ot)return L(e.Applicative0())(new Ot(o.value0));if(o instanceof Mt)return L(e.Applicative0())(new Mt(St(t)(u.value0)(o.value0)));throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): "+[o.constructor.name])});throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): "+[u.constructor.name])})}},Functor0:function(){return n_(e.Bind1().Apply0().Functor0())}}}};var Bv=function(){var t=In();return function(e){return t(Vp(e))}}();function Tg(t,e,r){return t==null?e:r(t)}var Xr=function(t){return Tg(t,z.value,B.create)};function u_(t){return Object.prototype.toString.call(t).slice(8,-1)}var Og=Array.isArray||function(t){return Object.prototype.toString.call(t)==="[object Array]"};var Jv=function(){function t(e,r){this.value0=e,this.value1=r}return t.create=function(e){return function(r){return new t(e,r)}},t}();var Ig=_t;var jv=function(t){var e=co(Sg(t));return function(r){return e(Jm(r))}};var Xv=function(t){return function(e){return function(r){if(u_(r)===e)return L(a_(t))(Ig(r));if(nr)return jv(t)(new Jv(e,u_(r)));throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): "+[e.constructor.name,r.constructor.name])}}};var Qv=function(t){return Xv(t)("String")};var Lg=function(){function t(){}return t.value=new t,t}(),Wg=function(){function t(){}return t.value=new t,t}(),GP=function(){function t(){}return t.value=new t,t}();var jp=function(){function t(){}return t.value=new t,t}(),Xp=function(){function t(){}return t.value=new t,t}(),qg=function(){function t(){}return t.value=new t,t}(),Gg=function(){function t(){}return t.value=new t,t}(),Yv=function(){function t(){}return t.value=new t,t}(),Bg=function(){function t(){}return t.value=new t,t}(),Ug=function(){function t(){}return t.value=new t,t}();var Hg=function(t){return t},zg=function(t){return t};var Vg=function(t){return t};var Jg=function(t){return t};var jg=function(t){return t};var Xg=function(t){return t},Qg=function(t){return t},Kg=function(t){return t},Yg=function(t){return t},Zg=function(t){return t};var Zv=function(){function t(){}return t.value=new t,t}(),tE=function(){function t(){}return t.value=new t,t}(),eE=function(){function t(){}return t.value=new t,t}(),tD=function(){function t(){}return t.value=new t,t}(),rE=function(){function t(){}return t.value=new t,t}();var Qp=function(t){return t};var $f=function(t){return t};var BP=function(t){return t},o_=function(t){return t};var eD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),rD=function(){function t(){}return t.value=new t,t}();var wf={toAudioOnOff:Y(Z)};var Qe=function(t){return function(e){return function(r){return{mix:function(n){return He()({reflectSymbol:function(){return"node"}})(D.value)(n)}}}}},nE=function(t){return function(e){return function(r){return{mix:function(n){return He()({reflectSymbol:function(){return"dynamicChannels"}})(D.value)(n)}}}}},aE=function(t){return function(e){return function(r){return{mix:function(n){return He()({reflectSymbol:function(){return"eventfulNode"}})(D.value)(n)}}}}};var W=function(t){return function(e){return function(r){return{mix:function(n){return He()({reflectSymbol:function(){return"fixedChannels"}})(D.value)(n)}}}}},Pf=function(t){return t.toAudioParameter},uE=function(t){return t.toAudioOnOff};var Ke=function(t){return t.mix};var Kp=function(){return gg(function(){var t=yg()()(za),e=kg({reflectSymbol:function(){return"o"}})()()(D.value)(vn);return function(r){return t(e(r))}}())},oE=function(t){return t};var UP=function(){var t=He()({reflectSymbol:function(){return"unit"}})(D.value);return function(e){return o_(t(e))}}();var HP=function(t){return function(e){return{toAudioParameter:function(r){return UP(r)}}}},iE=function(t){return function(e){return{toAudioParameter:function(){var r=Pf(HP(t)(e));return function(n){return r(BP(function(u){return{u}}(n)))}}()}}},fE=function(){return He()({reflectSymbol:function(){return"2x"}})(D.value)(void 0)}(),cE=function(){var t=He()({reflectSymbol:function(){return"sudden"}})(D.value);return function(e){return o_(t(e))}}();var lE={toAudioParameter:cE},Yp={toAudioParameter:function(t){return cE({n:t})}},nD=function(){return He()({reflectSymbol:function(){return"step"}})(D.value)(void 0)}();var aD=function(){return He()({reflectSymbol:function(){return"on"}})(D.value)(void 0)}(),i_={x:aD,o:0},lt=function(){return K(E)(en()(He()({reflectSymbol:function(){return"onOff"}})(D.value)(i_)))};var _E=function(){return He()({reflectSymbol:function(){return"off"}})(D.value)(void 0)}();var zP=function(){var t=He()({reflectSymbol:function(){return"numeric"}})(D.value);return function(e){return o_(t(e))}}();var Wr={toAudioParameter:zP};var Ro=function(){return He()({reflectSymbol:function(){return"linear"}})(D.value)(void 0)}();var pE=function(){return He()({reflectSymbol:function(){return"exponential"}})(D.value)(void 0)}(),VP=function(){var t=He()({reflectSymbol:function(){return"envelope"}})(D.value);return function(e){return o_(t(e))}}();var xn={toAudioParameter:VP},JP=function(){var t=He()({reflectSymbol:function(){return"cancel"}})(D.value);return function(e){return o_(t(e))}}();var sE={toAudioParameter:JP},dn=function(t){return function(e){return function(r){return function(n){var u=function(o){return o({parent:t,scope:e,raiseId:De(Pi(So(Xo)))})(r)};return Ar()()()({fixedChannels:function(o){return Lr(wt)(g)(u)(o)},eventfulNode:function(o){return Dr(E)(_(A)(dn(t)(e)(r))(o))},node:u,dynamicChannels:function(o){return It(function(c){return function(){var l=ge(Tu)(),p=gt(o)(function(s){return function(){var v=r.ids(),f=ge(L(tt)(void 0))(),b=r.ids(),ct=ge(L(tt)(void 0))(),vt=ge(z.value)(),Qt=ge(L(tt)(void 0))(),ee=r.ids(),xe=ge(Lg.value)(),Hr=gt(s)(function(it){return function(){var be=Me(xe)();if(it instanceof rD&&be instanceof Wg){je(GP.value)(xe)();var qt=H(J)(H(J)(H(J)(H(J)(nt(Ge)(Me(vt))(mr(tt)(ur)(function(Un){return aa(tt)(Dc)(t)(function(ba){return c(r.disconnectXFromY({from:Un,to:ba}))})})))(ra(Ge)(Me(f))))(ra(Ge)(Me(ct))))(na(oc(v))(l)))(na(oc(b))(l));return H(J)(je(qt)(Qt))(qt)()}if(it instanceof eD&&be instanceof Lg){je(Wg.value)(xe)();var kr=Du(),Rr=gt(it.value0({parent:t,scope:ee,raiseId:function(Un){return Le(C)(wu(Un)(kr))}})(r))(c)(),da=ci(kr)(function(Un){if(Un instanceof Ot)return function(){return je(new B(Un.value0))(vt)(),na(xu(b)(Rr))(l)(),je(Rr)(ct)()};if(Un instanceof Mt)return Ha(Un.value0);throw new Error("Failed pattern match at WAGS.Core (line 1263, column 46 - line 1268, column 47): "+[Un.constructor.name])})();return da()}return void 0}})();return je(Hr)(f)(),na(xu(v)(Hr))(l)(),ra(Ge)(Me(Qt))()}})();return function(){return nt(Ge)(Me(l))(Ko(bf)(So(Xo)))(),p()}}})}})(n)}}}};var jP=function(){function t(){}return t.value=new t,t}(),XP=function(){function t(){}return t.value=new t,t}(),QP=function(){function t(){}return t.value=new t,t}(),KP=function(){function t(){}return t.value=new t,t}(),YP=function(){function t(){}return t.value=new t,t}(),ZP=function(){function t(){}return t.value=new t,t}(),tM=function(){function t(){}return t.value=new t,t}(),eM=function(){function t(){}return t.value=new t,t}(),rM=function(){function t(){}return t.value=new t,t}(),nM=function(){function t(){}return t.value=new t,t}(),aM=function(){function t(){}return t.value=new t,t}(),uM=function(){function t(){}return t.value=new t,t}(),oM=function(){function t(){}return t.value=new t,t}(),iM=function(){function t(){}return t.value=new t,t}(),_i=function(t){return{toPeriodicOscSpec:function(e){return He()({reflectSymbol:function(){return"realImg"}})(D.value)({real:Oo(e.value0),img:Oo(e.value1)})}}};var Zp={toInitializeTriangleOsc:function(t){return Zg(function(e){return{frequency:e}}(t))}};var mE={toInitializeStereoPanner:function(t){return Yg(function(e){return{pan:e}}(t))}};var bc={toInitializeSquareOsc:function(t){return Kg(function(e){return{frequency:e}}(t))}};var Qi={toInitializeSinOsc:function(t){return Qg(function(e){return{frequency:e}}(t))}};var vE={toInitializeSawtoothOsc:function(t){return Xg(function(e){return{frequency:e}}(t))}};var uD={toInitializeRecorder:function(t){return Hg(function(e){return{cb:e}}(t))}};var f_={toInitializeMicrophone:function(t){return zg(function(e){return{microphone:e}}(t))}};var DE=function(t){return function(e){return{toInitializeIIRFilter:function(r){return function(n){return function(u){return{feedforward:Af(a)(Ca()(r.value0)),feedback:Af(a)(Ca()(r.value1))}}}}}}};var ot={toInitializeGain:function(t){return jg(function(e){return{gain:e}}(t))}};var dE={toInitializeConvolver:function(t){return Vg(function(e){return{buffer:e}}(t))}},ts={toInitializeConstant:function(t){return Jg(function(e){return{offset:e}}(t))}};var fM={convertOption:function(t){return function(e){return Y(Z)}}},c_={convertOption:function(t){return function(e){return Y(Z)}}},bE={convertOption:function(t){return function(e){return Y(Z)}}},yE={convertOption:function(t){return function(e){return fn}}},AE={convertOption:function(t){return function(e){return Y(Z)}}},pi={convertOption:function(t){return function(e){return Y(Z)}}},yc={convertOption:function(t){return function(e){return Y(Z)}}},Ac={convertOption:function(t){return function(e){return Y(Z)}}},kc={convertOption:function(t){return function(e){return Y(Z)}}},gc={convertOption:function(t){return function(e){return Y(Z)}}},Ec={convertOption:function(t){return function(e){return Y(Z)}}},kE={convertOption:function(t){return function(e){return Y(Z)}}},gE={convertOption:function(t){return function(e){return Y(Z)}}},EE={convertOption:function(t){return function(e){return Y(Z)}}},oD={convertOption:function(t){return function(e){return Y(Z)}}},Mf={convertOption:function(t){return function(e){return Y(Z)}}},l_={convertOption:function(t){return function(e){return Y(Z)}}},__={convertOption:function(t){return function(e){return Y(Z)}}};var Cc={convertOption:function(t){return function(e){return Y(Z)}}},CE={convertOption:function(t){return function(e){return Y(Z)}}},SE={convertOption:function(t){return function(e){return Y(Z)}}},hE={convertOption:function(t){return function(e){return Y(Z)}}},iD={convertOption:function(t){return function(e){return Y(Z)}}};var TE={convertOption:function(t){return function(e){return Y(Z)}}},fD={convertOption:function(t){return function(e){return Y(Z)}}},bn={convertOption:function(t){return function(e){return Y(Z)}}},cn={convertOption:function(t){return function(e){return Y(Z)}}},cD={convertOption:function(t){return function(e){return Y(Z)}}},es={convertOption:function(t){return function(e){return Y(Z)}}},cM=function(t){return t.toPeriodicOscSpec},si=function(t){return{convertOption:function(e){return function(r){return cM(t)}}}},lD=function(t){return t.toInitializeWaveShaper},xE=function(t){return t.toInitializeTriangleOsc},FE=function(t){return t.toInitializeStereoPanner},$E=function(t){return t.toInitializeSquareOsc},wE=function(t){return t.toInitializeSinOsc},PE=function(t){return t.toInitializeSawtoothOsc},ME=function(t){return t.toInitializeRecorder},_D=function(t){return t.toInitializePlayBuf},OE=function(t){return t.toInitializePeriodicOsc},IE=function(t){return t.toInitializePeaking},RE=function(t){return t.toInitializeNotch},NE=function(t){return t.toInitializeMicrophone},LE=function(t){return t.toInitializeLowshelf},pD=function(t){return t.toInitializeLowpass},sD=function(t){return t.toInitializeLoopBuf},WE=function(t){return t.toInitializeIIRFilter},qE=function(t){return t.toInitializeHighshelf},mD=function(t){return t.toInitializeHighpass},GE=function(t){return t.toInitializeGain},BE=function(t){return t.toInitializeDynamicsCompressor},vD=function(t){return t.toInitializeDelay},UE=function(t){return t.toInitializeConvolver},HE=function(t){return t.toInitializeConstant},DD=function(t){return t.toInitializeBandpass},dD=function(t){return t.toInitializeAllpass};var lM={oversample:fE},_M=function(t){return{toInitializeWaveShaper:function(e){return ma(t)(jP.value)(lM)(e)}}},zE={toInitializeWaveShaper:function(){var t=lD(_M(kt(At()(U(yt)(fM)()()()({reflectSymbol:function(){return"curve"}})))(bt()())));return function(e){return t(function(r){return{curve:r}}(e))}}()},pM={bufferOffset:0,playbackRate:1,duration:r_},p_=function(t){return{toInitializePlayBuf:function(e){return ma(t)(XP.value)(pM)(e)}}},Va={toInitializePlayBuf:function(){var t=_D(p_(kt(At()(U(yt)(c_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},sM={},mi=function(t){return{toInitializePeriodicOsc:function(e){return ma(t)(QP.value)(sM)(e)}}},mM={q:1,gain:0},Sc=function(t){return{toInitializePeaking:function(e){return ma(t)(KP.value)(mM)(e)}}};var vM={q:1},hc=function(t){return{toInitializeNotch:function(e){return ma(t)(YP.value)(vM)(e)}}};var DM={gain:0},VE=function(t){return{toInitializeLowshelf:function(e){return ma(t)(ZP.value)(DM)(e)}}};var dM={q:1},bD=function(t){return{toInitializeLowpass:function(e){return ma(t)(tM.value)(dM)(e)}}},rs={toInitializeLowpass:function(){var t=pD(bD(kt(At()(U(yt)(oD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},bM={loopStart:0,loopEnd:0,playbackRate:1,duration:r_},Of=function(t){return{toInitializeLoopBuf:function(e){return ma(t)(eM.value)(bM)(e)}}},Ce={toInitializeLoopBuf:function(){var t=sD(Of(kt(At()(U(yt)(Mf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())));return function(e){return t(function(r){return{buffer:r}}(e))}}()},yM={gain:0},JE=function(t){return{toInitializeHighshelf:function(e){return ma(t)(rM.value)(yM)(e)}}};var AM={q:1},yD=function(t){return{toInitializeHighpass:function(e){return ma(t)(nM.value)(AM)(e)}}},Ya={toInitializeHighpass:function(){var t=mD(yD(kt(At()(U(yt)(iD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},kM=function(){return{ratio:12,attack:.003,release:.25,knee:30,threshold:-24}}(),jE=function(t){return{toInitializeDynamicsCompressor:function(e){return ma(t)(aM.value)(kM)(e)}}},gM={maxDelayTime:1},AD=function(t){return{toInitializeDelay:function(e){return ma(t)(uM.value)(gM)(e)}}},Qr={toInitializeDelay:function(){var t=vD(AD(kt(At()(U(yt)(fD)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())));return function(e){return t(function(r){return{delayTime:r}}(e))}}()},EM={q:1},ln=function(t){return{toInitializeBandpass:function(e){return ma(t)(oM.value)(EM)(e)}}},kD={toInitializeBandpass:function(){var t=DD(ln(kt(At()(U(yt)(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()},CM={q:1},ns=function(t){return{toInitializeAllpass:function(e){return ma(t)(iM.value)(CM)(e)}}},gD={toInitializeAllpass:function(){var t=dD(ns(kt(At()(U(yt)(es)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())));return function(e){return t(function(r){return{frequency:r}}(e))}}()};function ED(t){return()=>t.slice()}function CD(t){return e=>r=>()=>{r[t]=e}}function SD(t){return()=>t.slice()}var Tc=su,xc=su,Fc=su,Za=su,tu=su,eu=su,ru=su,wa=su;var xM=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}};var FM=function(){function t(){}return t.value=new t,t}();var as={convertOption:function(t){return function(e){return Y(Z)}}},us={convertOption:function(t){return function(e){return Y(Z)}}};var QE=function(t){return function(e){return function(r){return function(n){var u=lD(t)(r),o=function(c){return function(i){return It(function(l){return function(){var s=i.ids();return c.raiseId(s)(),_(C)(function(m){return H(J)(l(i.deleteFromCache({id:s})))(m)})(ht(gt)(l)(P(R)(Tt(i.makeWaveShaper({id:s,parent:c.parent,scope:c.scope,curve:u.curve,oversample:u.oversample})))(dn(fn(s))(c.scope)(i)(Ke(e)(n)))))()}})}};return o}}}};var $M=function(t){return t.toInitializeAnalyser},wM=function(t){return function(e){return function(r){return It(function(n){return function(){var o=r.ids();return n(r.makeSpeaker({id:o}))(),gt(dn(fn(o))("toplevel")(r)(Ke(t)(e)))(n)()}})}}},If=function(t){return wM(t)},hD=function(t){return function(e){return function(r){var n=ME(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeRecorder({id:p,parent:o.parent,scope:o.scope,cb:n.cb})))(dn(fn(p))(o.scope)(c)(Ke(Qe(a)(a)(a))(r)))))()}})}};return u}}},PM=function(t){return function(e){return function(r){return function(n){var u=function(o){return function(c){return It(function(i){return function(){var p=ED(_(Pe)(S(""))(Oo(r)))(),s=tr(kv)(g)(Xu(yv)(function(Qt){return function(ee){return ee({parent:fn("@fan@"),scope:e(o.scope),raiseId:function(xe){return CD(Qt)(xe)(p)}})(c)}})(r)),m=gt(s)(i)(),v=Du(),f=_(C)(_t)(SD(p))(),b=_(Av)(function(Qt){return function(ee){return function(xe){return It(function(Hr){return function(){return ee.raiseId(Qt)(),aa(tt)(Dc)(ee.parent)(function(oe){return Hr(xe.connectXToY({from:Qt,to:oe}))})(),L(tt)(void 0)}})}}})(f),ct=dn(o.parent)(o.scope)(c)(n(b)(function(Qt){return Qt})),vt=gt(ct)(i)();return Le(C)(wu(vt)(v))(),function(){m(),En(tt)(!t)(Mi(Oo(f))(function(xe){return i(c.deleteFromCache({id:xe}))}))();var ee=ci(v)(function(xe){if(xe instanceof Ot)return xe.value0;if(xe instanceof Mt)return Ha(xe.value0);throw new Error("Failed pattern match at WAGS.Control (line 1615, column 36 - line 1617, column 35): "+[xe.constructor.name])})();return ee()}}})}};return u}}}},MM=function(){return function(){return function(t){return function(e){return function(r){return function(n){return function(u){return function(o){var c=WE(t)(u)(r)(n),i=function(l){return function(p){return It(function(s){return function(){var v=p.ids();return l.raiseId(v)(),_(C)(function(f){return H(J)(s(p.deleteFromCache({id:v})))(f)})(ht(gt)(s)(P(R)(Tt(p.makeIIRFilter({id:v,parent:l.parent,scope:l.scope,feedforward:Oo(c.feedforward),feedback:Oo(c.feedback)})))(dn(fn(v))(l.scope)(p)(Ke(e)(o)))))()}})}};return i}}}}}}}},KE=function(){return function(){return function(t){return function(e){return function(r){return MM()()(e)(t)(D.value)(D.value)}}}}};var Rt=function(t){return function(e){return function(r){return function(n){return Ur(t)(e)(r)(T(g))(n)}}}},Ur=function(t){return function(e){return function(r){return function(n){return function(u){var o=GE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeGain({id:m,parent:i.parent,scope:i.scope,gain:o.gain})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({gain:YE(601)(i.scope)(l)(function(f){return l.setGain(function(b){return{id:m,gain:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},YE=xM("tmpResolveAU","WAGS.Control",function(){var t=function(){var c=He()({reflectSymbol:function(){return"unit"}})(D.value);return function(i){return $f(c(i))}}(),e=function(){var c=He()({reflectSymbol:function(){return"sudden"}})(D.value);return function(i){return $f(c(i))}}(),r=function(){var c=He()({reflectSymbol:function(){return"numeric"}})(D.value);return function(i){return $f(c(i))}}(),n=function(){var c=He()({reflectSymbol:function(){return"envelope"}})(D.value);return function(i){return $f(c(i))}}(),u=function(){var c=He()({reflectSymbol:function(){return"cancel"}})(D.value);return function(i){return $f(c(i))}}(),o=function(c){return function(i){return function(l){return function(p){return Ar()()()({numeric:function(s){return Tt(l(r(s)))},envelope:function(s){return Tt(l(n(s)))},cancel:function(s){return Tt(l(u(s)))},sudden:function(s){return Tt(l(e(s)))},unit:function(s){var m=Rt(ot)(Qe(a)(a)(a))(1)(s.u);return It(function(v){return function(){var b=Du();return gt(P(R)(m({parent:r_,scope:c,raiseId:function(ct){return Le(C)(wu(ct)(b))}})(i))(It(function(ct){return function(){return Le(C)(ci(b)(function(Qt){if(Qt instanceof Mt)return Ha(Qt.value0);if(Qt instanceof Ot)return ct(l(t({i:Qt.value0})));throw new Error("Failed pattern match at WAGS.Control (line 1761, column 39 - line 1764, column 66): "+[Qt.constructor.name])}))(),L(tt)(void 0)}})))(v)()}})}})(p)}}}};return o}),lr=YE(1740),TD=function(t){return function(e){return function(r){return function(n){return function(u){var o=mD(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeHighpass({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},$c=function(t){return function(e){return function(r){return function(n){return TD(t)(e)(r)(T(g))(n)}}}},OM=function(t){return function(e){return function(r){return function(n){return function(u){var o=qE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeHighshelf({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,gain:o.gain})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),gain:lr(i.scope)(l)(function(f){return l.setGain(function(b){return{id:m,gain:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},ZE=function(t){return function(e){return function(r){return function(n){return OM(t)(e)(r)(T(g))(n)}}}},tC=function(t){return function(e){return function(r){return function(n){return function(u){var o=pD(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeLowpass({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},wc=function(t){return function(e){return function(r){return function(n){return tC(t)(e)(r)(T(g))(n)}}}},IM=function(t){return function(e){return function(r){return function(n){return function(u){var o=LE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeLowshelf({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,gain:o.gain})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),gain:lr(i.scope)(l)(function(f){return l.setGain(function(b){return{id:m,gain:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},eC=function(t){return function(e){return function(r){return function(n){return IM(t)(e)(r)(T(g))(n)}}}},RM=function(t){return function(e){return function(r){return function(n){return function(u){var o=RE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeNotch({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},Pc=function(t){return function(e){return function(r){return function(n){return RM(t)(e)(r)(T(g))(n)}}}},NM=function(t){return function(e){return function(r){return function(n){return function(u){var o=FE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeStereoPanner({id:m,parent:i.parent,scope:i.scope,pan:o.pan})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({pan:lr(i.scope)(l)(function(f){return l.setPan(function(b){return{id:m,pan:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},rC=function(t){return function(e){return function(r){return NM(t)(e)(r)(T(g))}}},LM=function(t){return function(e){return function(r){return function(n){return function(u){var o=IE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makePeaking({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q,gain:o.gain})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))}),gain:lr(i.scope)(l)(function(f){return l.setGain(function(b){return{id:m,gain:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},Mc=function(t){return function(e){return function(r){return function(n){return LM(t)(e)(r)(T(g))(n)}}}},nu=function(t){var e=function(r){return function(n){return It(function(u){return function(){var c=Du(),i=t(function(l){return function(p){return It(function(s){return function(){return Le(C)(Pk(c)(function(v){if(v instanceof Mt)return Ha(v.value0);if(v instanceof Ot)return aa(tt)(Dc)(l.parent)(function(f){return En(tt)(v.value0!==f)(H(J)(l.raiseId(v.value0))(s(n.connectXToY({from:v.value0,to:f}))))});throw new Error("Failed pattern match at WAGS.Control (line 1669, column 29 - line 1673, column 81): "+[v.constructor.name])}))(),L(tt)(void 0)}})}});return gt(i({parent:r.parent,scope:r.scope,raiseId:function(l){return function(){return r.raiseId(l)(),Le(C)(wu(l)(c))()}}})(n))(u)()}})}};return e};var WM=function(t){return PM(!1)(Y(Z))(t)},Pa=function(t){return function(e){return WM(Sk(t))(po(za)(ht(hf(yr)())(wa))(e))}},qM=function(t){return function(e){return function(r){return function(n){return function(u){var o=BE(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeDynamicsCompressor({id:m,parent:i.parent,scope:i.scope,threshold:o.threshold,ratio:o.ratio,knee:o.knee,attack:o.attack,release:o.release})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({threshold:lr(i.scope)(l)(function(f){return l.setThreshold(function(b){return{id:m,threshold:b}}(f))}),ratio:lr(i.scope)(l)(function(f){return l.setRatio(function(b){return{id:m,ratio:b}}(f))}),knee:lr(i.scope)(l)(function(f){return l.setKnee(function(b){return{id:m,knee:b}}(f))}),attack:lr(i.scope)(l)(function(f){return l.setAttack(function(b){return{id:m,attack:b}}(f))}),release:lr(i.scope)(l)(function(f){return l.setRelease(function(b){return{id:m,release:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},nC=function(t){return function(e){return function(r){return qM(t)(e)(r)(T(g))}}},s_=function(t){return function(e){return function(r){return function(n){return function(u){var o=vD(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeDelay({id:m,parent:i.parent,scope:i.scope,delayTime:o.delayTime,maxDelayTime:o.maxDelayTime})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({delayTime:lr(i.scope)(l)(function(f){return l.setDelay(function(b){return{id:m,delayTime:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},so=function(t){return function(e){return function(r){return function(n){return s_(t)(e)(r)(T(g))(n)}}}};var GM=function(){return{cb:function(t){return L(tt)(L(tt)(void 0))},fftSize:Yv.value,maxDecibels:-30,minDecibels:-100,smoothingTimeConstant:.8,channelCount:2,channelCountMode:tD.value,channelInterpretation:Zv.value}}(),os=function(t){return{toInitializeAnalyser:function(e){return ma(t)(FM.value)(GM)(e)}}};var aC=function(t){return function(e){return function(r){return function(n){var u=UE(t)(r),o=function(c){return function(i){return It(function(l){return function(){var s=i.ids();return c.raiseId(s)(),_(C)(function(m){return H(J)(l(i.deleteFromCache({id:s})))(m)})(ht(gt)(l)(P(R)(Tt(i.makeConvolver({id:s,parent:c.parent,scope:c.scope,buffer:u.buffer})))(dn(fn(s))(c.scope)(i)(Ke(e)(n)))))()}})}};return o}}}},xD=function(t){return function(e){return function(r){return function(n){return function(u){var o=DD(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeBandpass({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},yn=function(t){return function(e){return function(r){return function(n){return xD(t)(e)(r)(T(g))(n)}}}},BM=function(t){return function(e){return function(r){return function(n){return function(u){var o=$M(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeAnalyser({id:m,parent:i.parent,scope:i.scope,cb:o.cb,fftSize:Vm(2)(function(){if(o.fftSize instanceof jp)return 7;if(o.fftSize instanceof Xp)return 8;if(o.fftSize instanceof qg)return 9;if(o.fftSize instanceof Gg)return 10;if(o.fftSize instanceof Yv)return 11;if(o.fftSize instanceof Bg)return 12;if(o.fftSize instanceof Ug)return 13;throw new Error("Failed pattern match at WAGS.Control (line 190, column 21 - line 197, column 34): "+[o.fftSize.constructor.name])}()),maxDecibels:o.maxDecibels,minDecibels:o.minDecibels,smoothingTimeConstant:o.smoothingTimeConstant,channelCount:o.channelCount,channelCountMode:function(){if(o.channelCountMode instanceof rE)return"explicit";if(o.channelCountMode instanceof tD)return"max";if(o.channelCountMode instanceof eE)return"clamped-max";throw new Error("Failed pattern match at WAGS.Control (line 203, column 35 - line 206, column 46): "+[o.channelCountMode.constructor.name])}(),channelInterpretation:function(){if(o.channelInterpretation instanceof Zv)return"speakers";if(o.channelInterpretation instanceof tE)return"discrete";throw new Error("Failed pattern match at WAGS.Control (line 207, column 40 - line 209, column 41): "+[o.channelInterpretation.constructor.name])}()})))(P(R)(_(A)(function(v){return Ar()()()({cb:function(f){return l.setAnalyserNodeCb({id:m,cb:f})}})(v)})(n))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},is=function(t){return function(e){return function(r){return BM(t)(e)(r)(T(g))}}},UM=function(t){return function(e){return function(r){return function(n){return function(u){var o=dD(t)(r),c=function(i){return function(l){return It(function(p){return function(){var m=l.ids();return i.raiseId(m)(),_(C)(function(v){return H(J)(p(l.deleteFromCache({id:m})))(v)})(ht(gt)(p)(P(R)(Tt(l.makeAllpass({id:m,parent:i.parent,scope:i.scope,frequency:o.frequency,q:o.q})))(P(R)(Dr(E)(_(A)(function(v){return Ar()()()({frequency:lr(i.scope)(l)(function(f){return l.setFrequency(function(b){return{id:m,frequency:b}}(f))}),q:lr(i.scope)(l)(function(f){return l.setQ(function(b){return{id:m,q:b}}(f))})})(v)})(n)))(dn(fn(m))(i.scope)(l)(Ke(e)(u))))))()}})}};return c}}}}},m_=function(t){return function(e){return function(r){return function(n){return UM(t)(e)(r)(T(g))(n)}}}},HM=function(t){return function(e){return function(r){var n=xE(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeTriangleOsc({id:p,parent:o.parent,scope:o.scope,frequency:n.frequency})))(Dr(E)(_(A)(function(s){return Ar()()()({frequency:lr(o.scope)(c)(function(m){return c.setFrequency(function(v){return{id:p,frequency:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},fs=function(t){return HM(t)};var zM=function(t){return function(e){return function(r){var n=$E(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeSquareOsc({id:p,parent:o.parent,scope:o.scope,frequency:n.frequency})))(Dr(E)(_(A)(function(s){return Ar()()()({frequency:lr(o.scope)(c)(function(m){return c.setFrequency(function(v){return{id:p,frequency:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},v_=function(t){return zM(t)},uC=function(t){return function(e){return v_(t)(e)(T(g))}},VM=function(t){return function(e){return function(r){var n=wE(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeSinOsc({id:p,parent:o.parent,scope:o.scope,frequency:n.frequency})))(Dr(E)(_(A)(function(s){return Ar()()()({frequency:lr(o.scope)(c)(function(m){return c.setFrequency(function(v){return{id:p,frequency:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},Rf=function(t){return VM(t)},oC=function(t){return function(e){return Rf(t)(e)(T(g))}},JM=function(t){return function(e){return function(r){var n=PE(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeSawtoothOsc({id:p,parent:o.parent,scope:o.scope,frequency:n.frequency})))(Dr(E)(_(A)(function(s){return Ar()()()({frequency:lr(o.scope)(c)(function(m){return c.setFrequency(function(v){return{id:p,frequency:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},iC=function(t){return JM(t)};var jM=function(t){return function(e){return function(r){var n=_D(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makePlayBuf({id:p,parent:o.parent,scope:o.scope,buffer:n.buffer,playbackRate:n.playbackRate,bufferOffset:n.bufferOffset,duration:n.duration})))(Dr(E)(_(A)(function(s){return Ar()()()({buffer:function(m){return Tt(c.setBuffer({id:p,buffer:m}))},playbackRate:lr(o.scope)(c)(function(m){return c.setPlaybackRate(function(v){return{id:p,playbackRate:v}}(m))}),bufferOffset:function(m){return Tt(c.setBufferOffset({id:p,bufferOffset:m}))},onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))},duration:function(m){return Tt(c.setDuration({id:p,duration:m}))}})(s)})(r)))))()}})}};return u}}},Gn=function(t){return jM(t)};var XM=function(t){return function(e){return function(r){var n=OE(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makePeriodicOsc({id:p,parent:o.parent,scope:o.scope,frequency:n.frequency,spec:n.spec})))(Dr(E)(_(A)(function(s){return Ar()()()({frequency:lr(o.scope)(c)(function(m){return c.setFrequency(function(v){return{id:p,frequency:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))},spec:function(m){return Tt(c.setPeriodicOsc({id:p,spec:m}))}})(s)})(r)))))()}})}};return u}}},vi=function(t){return XM(t)};var QM=function(t){return function(e){var r=NE(t)(e),n=function(u){return function(o){return It(function(c){return function(){var l=o.ids();return u.raiseId(l)(),_(C)(function(p){return H(J)(c(o.deleteFromCache({id:l})))(p)})(ht(gt)(c)(Tt(o.makeMicrophone({id:l,parent:u.parent,scope:u.scope,microphone:r.microphone}))))()}})}};return n}},D_=function(t){return QM(t)};var KM=function(t){return function(e){return function(r){var n=sD(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeLoopBuf({id:p,parent:o.parent,scope:o.scope,buffer:n.buffer,playbackRate:n.playbackRate,loopStart:n.loopStart,loopEnd:n.loopEnd,duration:n.duration})))(Dr(E)(_(A)(function(s){return Ar()()()({buffer:function(m){return Tt(c.setBuffer({id:p,buffer:m}))},playbackRate:lr(o.scope)(c)(function(m){return c.setPlaybackRate(function(v){return{id:p,playbackRate:v}}(m))}),loopStart:function(m){return Tt(c.setLoopStart({id:p,loopStart:m}))},loopEnd:function(m){return Tt(c.setLoopEnd({id:p,loopEnd:m}))},onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},ue=function(t){return KM(t)};var YM=function(t){return function(e){return function(r){var n=HE(t)(e),u=function(o){return function(c){return It(function(i){return function(){var p=c.ids();return o.raiseId(p)(),_(C)(function(s){return H(J)(i(c.deleteFromCache({id:p})))(s)})(ht(gt)(i)(P(R)(Tt(c.makeConstant({id:p,parent:o.parent,scope:o.scope,offset:n.offset})))(Dr(E)(_(A)(function(s){return Ar()()()({offset:lr(o.scope)(c)(function(m){return c.setOffset(function(v){return{id:p,offset:v}}(m))}),onOff:function(m){return Tt(c.setOnOff({id:p,onOff:m}))}})(s)})(r)))))()}})}};return u}}},cs=function(t){return YM(t)};function FD(){window.scrollTo(0,0)}var mo=function(t){return t.sequential},Fn=function(t){return t.parallel};var An=function(t){return function(e){return function(r){return V("button")(e)(F(t)(r))}}};var Ma=function(){var t={},e="Pure",r="Throw",n="Catch",u="Sync",o="Async",c="Bind",i="Bracket",l="Fork",p="Sequential",s="Map",m="Apply",v="Alt",f="Cons",b="Resume",ct="Release",vt="Finalizer",Qt="Finalized",ee="Forked",xe="Fiber",Hr="Thunk";function it(Gt,er,zr,pr){this.tag=Gt,this._1=er,this._2=zr,this._3=pr}function oe(Gt){var er=function(zr,pr,re){return new it(Gt,zr,pr,re)};return er.tag=Gt,er}function be(Gt){return new it(e,void 0)}function qt(Gt){try{Gt()}catch(er){setTimeout(function(){throw er},0)}}function kr(Gt,er,zr){try{return er(zr())}catch(pr){return Gt(pr)}}function Rr(Gt,er,zr){try{return er(zr)()}catch(pr){return zr(Gt(pr))(),be}}var da=function(){var Gt=1024,er=0,zr=0,pr=new Array(Gt),re=!1;function Ct(){var or;for(re=!0;er!==0;)er--,or=pr[zr],pr[zr]=void 0,zr=(zr+1)%Gt,or();re=!1}return{isDraining:function(){return re},enqueue:function(or){var Ie,qr;er===Gt&&(qr=re,Ct(),re=qr),pr[(zr+er)%Gt]=or,er++,re||Ct()}}}();function Un(Gt){var er={},zr=0,pr=0;return{register:function(re){var Ct=zr++;re.onComplete({rethrow:!0,handler:function(or){return function(){pr--,delete er[Ct]}}})(),er[Ct]=re,pr++},isEmpty:function(){return pr===0},killAll:function(re,Ct){return function(){if(pr===0)return Ct();var or=0,Ie={};function qr(br){Ie[br]=er[br].kill(re,function(Kr){return function(){delete Ie[br],or--,Gt.isLeft(Kr)&&Gt.fromLeft(Kr)&&setTimeout(function(){throw Gt.fromLeft(Kr)},0),or===0&&Ct()}})()}for(var Hn in er)er.hasOwnProperty(Hn)&&(or++,qr(Hn));return er={},zr=0,pr=0,function(br){return new it(u,function(){for(var Kr in Ie)Ie.hasOwnProperty(Kr)&&Ie[Kr]()})}}}}}var ba=0,pn=1,Bo=2,Bf=3,Uf=4,Fr=5,Uo=6;function Hf(Gt,er,zr){var pr=0,re=ba,Ct=zr,or=null,Ie=null,qr=null,Hn=null,br=null,Kr=0,of=0,ku=null,Ti=!0;function xi(ne){for(var ie,ze,Ye;;)switch(ie=null,ze=null,Ye=null,re){case Bo:re=pn;try{Ct=qr(Ct),Hn===null?qr=null:(qr=Hn._1,Hn=Hn._2)}catch(ta){re=Fr,or=Gt.left(ta),Ct=null}break;case Bf:Gt.isLeft(Ct)?(re=Fr,or=Ct,Ct=null):qr===null?re=Fr:(re=Bo,Ct=Gt.fromRight(Ct));break;case pn:switch(Ct.tag){case c:qr&&(Hn=new it(f,qr,Hn)),qr=Ct._2,re=pn,Ct=Ct._1;break;case e:qr===null?(re=Fr,Ct=Gt.right(Ct._1)):(re=Bo,Ct=Ct._1);break;case u:re=Bf,Ct=kr(Gt.left,Gt.right,Ct._1);break;case o:re=Uf,Ct=Rr(Gt.left,Ct._1,function(ta){return function(){pr===ne&&(pr++,da.enqueue(function(){pr===ne+1&&(re=Bf,Ct=ta,xi(pr))}))}});return;case r:re=Fr,or=Gt.left(Ct._1),Ct=null;break;case n:qr===null?br=new it(f,Ct,br,Ie):br=new it(f,Ct,new it(f,new it(b,qr,Hn),br,Ie),Ie),qr=null,Hn=null,re=pn,Ct=Ct._1;break;case i:Kr++,qr===null?br=new it(f,Ct,br,Ie):br=new it(f,Ct,new it(f,new it(b,qr,Hn),br,Ie),Ie),qr=null,Hn=null,re=pn,Ct=Ct._1;break;case l:re=Bf,ie=Hf(Gt,er,Ct._2),er&&er.register(ie),Ct._1&&ie.run(),Ct=Gt.right(ie);break;case p:re=pn,Ct=Zh(Gt,er,Ct._1);break}break;case Fr:if(qr=null,Hn=null,br===null)re=Uo,Ct=Ie||or||Ct;else switch(ie=br._3,Ye=br._1,br=br._2,Ye.tag){case n:Ie&&Ie!==ie&&Kr===0?re=Fr:or&&(re=pn,Ct=Ye._2(Gt.fromLeft(or)),or=null);break;case b:Ie&&Ie!==ie&&Kr===0||or?re=Fr:(qr=Ye._1,Hn=Ye._2,re=Bo,Ct=Gt.fromRight(Ct));break;case i:Kr--,or===null&&(ze=Gt.fromRight(Ct),br=new it(f,new it(ct,Ye._2,ze),br,ie),(Ie===ie||Kr>0)&&(re=pn,Ct=Ye._3(ze)));break;case ct:br=new it(f,new it(Qt,Ct,or),br,Ie),re=pn,Ie&&Ie!==ie&&Kr===0?Ct=Ye._1.killed(Gt.fromLeft(Ie))(Ye._2):or?Ct=Ye._1.failed(Gt.fromLeft(or))(Ye._2):Ct=Ye._1.completed(Gt.fromRight(Ct))(Ye._2),or=null,Kr++;break;case vt:Kr++,br=new it(f,new it(Qt,Ct,or),br,Ie),re=pn,Ct=Ye._1;break;case Qt:Kr--,re=Fr,Ct=Ye._1,or=Ye._2;break}break;case Uo:for(var Jr in ku)ku.hasOwnProperty(Jr)&&(Ti=Ti&&ku[Jr].rethrow,qt(ku[Jr].handler(Ct)));ku=null,Ie&&or?setTimeout(function(){throw Gt.fromLeft(or)},0):Gt.isLeft(Ct)&&Ti&&setTimeout(function(){if(Ti)throw Gt.fromLeft(Ct)},0);return;case ba:re=pn;break;case Uf:return}}function Vr(ne){return function(){if(re===Uo)return Ti=Ti&&ne.rethrow,ne.handler(Ct)(),function(){};var ie=of++;return ku=ku||{},ku[ie]=ne,function(){ku!==null&&delete ku[ie]}}}function me(ne,ie){return function(){if(re===Uo)return ie(Gt.right(void 0))(),function(){};var ze=Vr({rethrow:!1,handler:function(){return ie(Gt.right(void 0))}})();switch(re){case ba:Ie=Gt.left(ne),re=Uo,Ct=Ie,xi(pr);break;case Uf:Ie===null&&(Ie=Gt.left(ne)),Kr===0&&(re===Uf&&(br=new it(f,new it(vt,Ct(ne)),br,Ie)),re=Fr,Ct=null,or=null,xi(++pr));break;default:Ie===null&&(Ie=Gt.left(ne)),Kr===0&&(re=Fr,Ct=null,or=null)}return ze}}function Ne(ne){return function(){var ie=Vr({rethrow:!1,handler:ne})();return re===ba&&xi(pr),ie}}return{kill:me,join:Ne,onComplete:Vr,isSuspended:function(){return re===ba},run:function(){re===ba&&(da.isDraining()?xi(pr):da.enqueue(function(){xi(pr)}))}}}function Ho(Gt,er,zr,pr){var re=0,Ct={},or=0,Ie={},qr=new Error("[ParAff] Early exit"),Hn=null,br=t;function Kr(Vr,me,Ne){var ne=me,ie=null,ze=null,Ye=0,Jr={},ta,Zc;t:for(;;)switch(ta=null,ne.tag){case ee:if(ne._3===t&&(ta=Ct[ne._1],Jr[Ye++]=ta.kill(Vr,function(t0){return function(){Ye--,Ye===0&&Ne(t0)()}})),ie===null)break t;ne=ie._2,ze===null?ie=null:(ie=ze._1,ze=ze._2);break;case s:ne=ne._2;break;case m:case v:ie&&(ze=new it(f,ie,ze)),ie=ne,ne=ne._1;break}if(Ye===0)Ne(Gt.right(void 0))();else for(Zc=0,ta=Ye;Zc<ta;Zc++)Jr[Zc]=Jr[Zc]();return Jr}function of(Vr,me,Ne){var ne,ie,ze,Ye,Jr,ta;Gt.isLeft(Vr)?(ne=Vr,ie=null):(ie=Vr,ne=null);t:for(;;){if(ze=null,Ye=null,Jr=null,ta=null,Hn!==null)return;if(me===null){pr(ne||ie)();return}if(me._3!==t)return;switch(me.tag){case s:ne===null?(me._3=Gt.right(me._1(Gt.fromRight(ie))),ie=me._3):me._3=ne;break;case m:if(ze=me._1._3,Ye=me._2._3,ne){if(me._3=ne,Jr=!0,ta=or++,Ie[ta]=Kr(qr,ne===ze?me._2:me._1,function(){return function(){delete Ie[ta],Jr?Jr=!1:Ne===null?of(ne,null,null):of(ne,Ne._1,Ne._2)}}),Jr){Jr=!1;return}}else{if(ze===t||Ye===t)return;ie=Gt.right(Gt.fromRight(ze)(Gt.fromRight(Ye))),me._3=ie}break;case v:if(ze=me._1._3,Ye=me._2._3,ze===t&&Gt.isLeft(Ye)||Ye===t&&Gt.isLeft(ze))return;if(ze!==t&&Gt.isLeft(ze)&&Ye!==t&&Gt.isLeft(Ye))ne=ie===ze?Ye:ze,ie=null,me._3=ne;else if(me._3=ie,Jr=!0,ta=or++,Ie[ta]=Kr(qr,ie===ze?me._2:me._1,function(){return function(){delete Ie[ta],Jr?Jr=!1:Ne===null?of(ie,null,null):of(ie,Ne._1,Ne._2)}}),Jr){Jr=!1;return}break}Ne===null?me=null:(me=Ne._1,Ne=Ne._2)}}function ku(Vr){return function(me){return function(){delete Ct[Vr._1],Vr._3=me,of(me,Vr._2._1,Vr._2._2)}}}function Ti(){var Vr=pn,me=zr,Ne=null,ne=null,ie,ze;t:for(;;)switch(ie=null,ze=null,Vr){case pn:switch(me.tag){case s:Ne&&(ne=new it(f,Ne,ne)),Ne=new it(s,me._1,t,t),me=me._2;break;case m:Ne&&(ne=new it(f,Ne,ne)),Ne=new it(m,t,me._2,t),me=me._1;break;case v:Ne&&(ne=new it(f,Ne,ne)),Ne=new it(v,t,me._2,t),me=me._1;break;default:ze=re++,Vr=Fr,ie=me,me=new it(ee,ze,new it(f,Ne,ne),t),ie=Hf(Gt,er,ie),ie.onComplete({rethrow:!1,handler:ku(me)})(),Ct[ze]=ie,er&&er.register(ie)}break;case Fr:if(Ne===null)break t;Ne._1===t?(Ne._1=me,Vr=pn,me=Ne._2,Ne._2=t):(Ne._2=me,me=Ne,ne===null?Ne=null:(Ne=ne._1,ne=ne._2))}for(br=me,ze=0;ze<re;ze++)Ct[ze].run()}function xi(Vr,me){Hn=Gt.left(Vr);var Ne;for(var ne in Ie)if(Ie.hasOwnProperty(ne)){Ne=Ie[ne];for(ne in Ne)Ne.hasOwnProperty(ne)&&Ne[ne]()}Ie=null;var ie=Kr(Vr,br,me);return function(ze){return new it(o,function(Ye){return function(){for(var Jr in ie)ie.hasOwnProperty(Jr)&&ie[Jr]();return be}})}}return Ti(),function(Vr){return new it(o,function(me){return function(){return xi(Vr,me)}})}}function Zh(Gt,er,zr){return new it(o,function(pr){return function(){return Ho(Gt,er,zr,pr)}})}return it.EMPTY=t,it.Pure=oe(e),it.Throw=oe(r),it.Catch=oe(n),it.Sync=oe(u),it.Async=oe(o),it.Bind=oe(c),it.Bracket=oe(i),it.Fork=oe(l),it.Seq=oe(p),it.ParMap=oe(s),it.ParApply=oe(m),it.ParAlt=oe(v),it.Fiber=Hf,it.Supervisor=Un,it.Scheduler=da,it.nonCanceler=be,it}(),fC=Ma.Pure,oO=Ma.Throw;function cC(t){return function(e){return e.tag===Ma.Pure.tag?Ma.Pure(t(e._1)):Ma.Bind(e,function(r){return Ma.Pure(t(r))})}}function lC(t){return function(e){return Ma.Bind(t,e)}}var _C=Ma.Sync;function pC(t){return function(e){return Ma.ParMap(t,e)}}function sC(t){return function(e){return Ma.ParApply(t,e)}}function mC(t){return function(e){return Ma.ParAlt(t,e)}}var Oc=Ma.Async;function vC(t,e){return function(){return Ma.Fiber(t,null,e)}}var iO=function(){function t(r,n){return r===0&&typeof setImmediate<"u"?setImmediate(n):setTimeout(n,r)}function e(r,n){return r===0&&typeof clearImmediate<"u"?clearImmediate(n):clearTimeout(n)}return function(r,n){return Ma.Async(function(u){return function(){var o=t(n,u(r()));return function(){return Ma.Sync(function(){return r(e(n,o))})}}})}}(),DC=Ma.Seq;var cO=function(t){return function(e){return function(r){var n=mo(t),u=mr(t.Applicative1())(e)(function(){var o=Fn(t);return function(c){return o(r(c))}}());return function(o){return n(u(o))}}}},dC=function(t){return function(e){return function(r){var n=mo(t),u=Jn(e)(t.Applicative1())(function(){var o=Fn(t);return function(c){return o(r(c))}}());return function(o){return n(u(o))}}}},bC=function(t){return function(e){return cO(t)(e)(Y(Z))}};var lO=function(t){return t};var AC=function(t){return t};var b_=function(t){return t.toDuration};var kC={fromDuration:lm()()(lO)(function(t){return t*1e3}),toDuration:lm()()(AC)(function(t){return t/1e3})};var gC=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}};var pO=function(t){return t};var Rc={map:pC},Di={map:cC};var sO=function(){var t=function(n){if(n instanceof Ot)return n.value0;if(n instanceof Mt)return pu("unsafeFromRight: Left");throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): "+[n.constructor.name])},e=function(n){if(n instanceof Mt)return n.value0;if(n instanceof Ot)return pu("unsafeFromLeft: Right");throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): "+[n.constructor.name])},r=function(n){if(n instanceof Mt)return!0;if(n instanceof Ot)return!1;throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): "+[n.constructor.name])};return{isLeft:r,fromLeft:e,fromRight:t,left:Mt.create,right:Ot.create}}(),mO=function(t){return vC(sO,t)},vo=function(t){return function(){var r=mO(t)();return r.run(),r}},Lo=function(){var t=Le(C);return function(e){return t(vo(e))}}();var di={apply:sC,Functor0:function(){return Rc}};var $D={Applicative0:function(){return va},Bind1:function(){return Ir}},Ir={bind:lC,Apply0:function(){return wD(0)}},va={pure:fC,Apply0:function(){return wD(0)}},wD=gC("applyAff","Effect.Aff",function(){return{apply:Bu($D),Functor0:function(){return Di}}}),EC=wD(71);var Or={liftEffect:_C,Monad0:function(){return $D}},CC=function(){var t=vr(Or);return function(e){return pO(S(t(e)))}}(),SC=function(t){return Oc(function(e){return _(C)(CC)(t.join(e))})};var hC=function(t){return function(e){return nt(Ir)(vr(Or)(e.isSuspended))(function(r){return r?vr(Or)(Le(C)(e.kill(t,S(L(tt)(void 0))))):Oc(function(n){return _(C)(CC)(e.kill(t,n))})})}};var $n={parallel:_t,sequential:DC,Monad0:function(){return $D},Applicative1:function(){return vO(0)}},vO=gC("applicativeParAff","Effect.Aff",function(){return{pure:function(){var t=Fn($n),e=L(va);return function(r){return t(e(r))}}(),Apply0:function(){return di}}});var DO={append:function(t){return function(e){return function(r){return bC($n)(wt)([t(r),e(r)])}}}};var dO=S(L(va)(void 0)),TC={mempty:dO,Semigroup0:function(){return DO}};var xC={alt:mC,Functor0:function(){return Rc}};var FC=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),y_=function(){function t(){}return t.value=new t,t}(),Nf=function(){function t(){}return t.value=new t,t}(),A_=function(){function t(){}return t.value=new t,t}(),Lf=function(){function t(){}return t.value=new t,t}(),k_=function(){function t(){}return t.value=new t,t}(),g_=function(){function t(){}return t.value=new t,t}(),$C=function(){function t(){}return t.value=new t,t}(),ls=function(){function t(){}return t.value=new t,t}(),_s=function(){function t(){}return t.value=new t,t}(),E_=function(){function t(){}return t.value=new t,t}(),C_=function(){function t(){}return t.value=new t,t}(),wC=function(){function t(){}return t.value=new t,t}(),Nc=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),PD=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}();var bO=function(t){for(var e="",r="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",n=r.length,u=0;u<t;u++)e+=r.charAt(Math.floor(Math.random()*n));return e},yO="numeric",AO="sudden",kO="unit",gO="cancel",EO="step",CO="linear",SO="exponential",hO="envelope",PC=function(t,e,r,n){if(r.type===AO)t.value=r.value.n;else if(r.type===kO)e.id&&xO(e.id,n),n.units[r.value.i].main.connect(t),e.id=r.value.i;else if(r.type===yO)t[r.value.t.type===EO?"setValueAtTime":r.value.t.type===CO?"linearRampToValueAtTime":r.value.t.type===SO?"exponentialRampToValueAtTime":"linearRampToValueAtTime"](r.value.n,r.value.o);else if(r.type===gO)r.value.hold?t.cancelAndHoldAtTime(r.value.o):t.cancelScheduledValues(r.value.o);else if(r.type===hO){let u=r.value.o;t.cancelScheduledValues(Math.max(0,u)),t.setValueCurveAtTime(r.value.p,u,r.value.d)}else throw new Error("No idea what to do with "+JSON.stringify(r))},TO=function(t,e,r,n,u){return n[r]||(n[r]={}),PC(e.parameters.get(r),n[r],u,t)},Ou=function(t,e,r,n,u){return n[r]||(n[r]={}),PC(e[r],n[r],u,t)},Er=function(t,e,r){r.scopes[e.value]||(r.scopes[e.value]=[]),r.scopes[e.value].push(t),r.units[t].scope=e},Cr=function(t,e){e.toConnect[t]&&(e.toConnect[t].forEach(function(r){r.w?e.units[r.w]?r.f():(e.toConnect[r.w]||(e.toConnect[r.w]=[]),e.toConnect[r.w].push({f:r.f})):r.f()}),delete e.toConnect[t])},Sr=function(t,e,r){e.type==="just"&&MC(t,e.value,r)},MC=function(t,e,r){var n=function(){r.units[t].audioOutgoing.push(e),r.units[t].pendingOn||(r.units[t].main.connect(r.units[e].main),r.units[e].se&&r.units[t].main.connect(r.units[e].se))};if(!r.units[t]){r.toConnect[t]||(r.toConnect[t]=[]);var u={f:n};e!==t&&!r.units[e]&&(u.w=e),r.toConnect[t].push(u);return}if(!r.units[e]){r.toConnect[e]||(r.toConnect[e]=[]);var u={f:n};e!==t&&!r.units[t]&&(u.w=t),r.toConnect[e].push(u);return}n()};function MD(t){return function(e){return function(){delete e.units[t.id]}}}function OD(t){return function(e){return function(){MC(t.from,t.to,e)}}}var xO=function(t,e){if(e.units[t].scope==="@fan@")return;let r=e.units[t].scope;e.scopes[r].forEach(n=>{delete e.units[n]}),delete e.scopes[r]};function ID(t){return function(e){return function(){var r=t.from,n=t.to;if(e.units[r].audioOutgoing=e.units[r].audioOutgoing.filter(function(o){return o!==n}),e.units[r].main.disconnect(e.units[n].main),e.units[n].se&&e.units[r].main.disconnect(e.units[n].se),e.units[ptr].scope==="@fan@")return;let u=e.units[ptr].scope;e.scopes[u].forEach(o=>{delete e.units[o]}),delete e.scopes[u]}}}function RD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"allpass",Q:t.q,frequency:t.frequency})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ND(t){return function(e){return function(){var r=t.id,n=t.cb,u=new AnalyserNode(e.context,t),o=n(u)();e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],analyserOrig:n,analyser:o,main:e.context.createGain(),se:u},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function LD(t){return function(e){return function(){var r=t.id,n=t.options;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new AudioWorkletNode(e.context,n.name,{numberOfInputs:n.numberOfInputs,numberOfOutputs:n.numberOfOutputs,outputChannelCount:n.outputChannelCount,parameterData:n.parameterData,processorOptions:n.processorOptions})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function WD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"bandpass",Q:t.q,frequency:t.frequency})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function qD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new ConstantSourceNode(o,c)},u={offset:t.offset};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function GD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new ConvolverNode(e.context,{buffer:t.buffer})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function BD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DelayNode(e.context,{delayTime:t.delayTime,maxDelayTime:t.maxDelayTime})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function UD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new DynamicsCompressorNode(e.context,{knee:t.knee,ratio:t.ratio,threshold:t.threshold,attack:t.attack,release:t.release})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}var HD=function(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new GainNode(e.context,{gain:t.gain})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}};function zD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highpass",Q:t.q,frequency:t.frequency})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function VD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"highshelf",frequency:t.frequency,gain:t.gain})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function JD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new IIRFilterNode(e.context,{feedforward:t.feedforward,feedback:t.feedback})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function jD(t){return function(e){return function(){var r=t.id,n=function(o,c){return new AudioBufferSourceNode(o,c)},u={loop:!0,buffer:t.buffer,loopStart:t.loopStart,loopEnd:t.loopEnd,playbackRate:t.playbackRate};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function XD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowpass",Q:t.q,frequency:t.frequency})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function QD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"lowshelf",frequency:t.frequency,gain:t.gain})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function KD(t){return function(e){return function(){var r=t.id,n=t.element,u=function(){var o=e.context.createMediaElementSource(n);return o};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],createClosure:u,resumeClosure:{},main:u()},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function YD(t){return function(e){return function(){var r=t.id;e.units[t.id]={main:e.context.createMediaStreamSource(t.microphone),controllers:{},audioOutgoing:[],controlOutgoing:[]},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ZD(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"notch",frequency:t.frequency,Q:t.q})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function td(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new BiquadFilterNode(e.context,{type:"peaking",frequency:t.frequency,Q:t.q,gain:t.gain})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ed(t){return function(e){return function(){var r=t.id,n=function(o,c){var i={frequency:c.frequency,periodicWave:c.spec.type==="wave"?c.spec.value:Wd(e.context)(c.spec.value.real)(c.spec.value.img)()},l=new OscillatorNode(o,i);return l},u={frequency:t.frequency,type:"custom",spec:t.spec};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function rd(t){return function(e){return function(){var r=t.id,n=function(o,c){var i={loop:c.loop,buffer:c.buffer,playbackRate:c.playbackRate};return new AudioBufferSourceNode(o,i)},u={loop:!1,buffer:t.buffer,playbackRate:t.playbackRate,bufferOffset:t.bufferOffset,duration:t.duration};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function nd(t){return function(e){return function(){var r=t.id,n=t.cb,u=e.context.createMediaStreamDestination(),o=new MediaRecorder(u.stream);n(o)(),o.start(),e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],recorderOrig:n,recorder:o,main:e.context.createGain(),se:u},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ad(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"sawtooth"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ud(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"sine"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function od(t){return function(e){return function(){e.units[t.id]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:e.context.createGain(),se:e.context.destination}}}}function id(t){return function(e){return function(){var r=t.id;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new StereoPannerNode(e.context,{pan:t.pan})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function fd(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"square"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function cd(t){return function(e){return function(){var r=t.id,n=function(o,c){return new OscillatorNode(o,c)},u={frequency:t.frequency,type:"triangle"};e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],resume:u,createClosure:n,onOff:!1,pendingOn:!0,main:n(e.context,u)},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function ld(t){return function(e){return function(){var r=t.id,n=t.curve,u=t.oversample;e.units[r]={controllers:{},audioOutgoing:[],controlOutgoing:[],main:new WaveShaperNode(e.context,{curve:n,oversample:u.type})},Er(r,t.scope,e),Cr(r,e),Sr(r,t.parent,e)}}}function _d(t){return function(e){return function(){var r=t.id,n=t.cb;e.units[r].analyserOrig!==n&&(e.units[r].analyser&&e.units[r].analyser(),e.units[r].analyser=n(e.units[r].se)(),e.units[r].analyserOrig=n)}}}function pd(t){return function(e){return function(){var r=t.cb,n=t.id;if(e.units[n].recorderOrig!==r){e.units[n].recorder&&e.units[n].recorder.stop();var u=r;e.units[n].recorderOrig=r;var o=new MediaRecorder(e.units[n].se);u(o)(),o.start()}}}}function sd(t){return function(e){return function(){var r=t.id,n=t.curve;e.units[r].main.curve=n}}}function md(t){return function(e){return function(){var r=t.id,n=t.paramName,u=t.paramValue;TO(e,e.units[r].main,n,e.units[r].controllers,u)}}}var Iu=function(t,e,r){e.resume&&t.value.n!==void 0&&(e.resume[r]=t.value.n)};function vd(t){return function(e){return function(){var r=t.id,n=t.gain;Ou(e,e.units[r].main,"gain",e.units[r].controllers,n),Iu(n,e.units[r],"gain")}}}function Dd(t){return function(e){return function(){var r=t.id,n=t.q;Ou(e,e.units[r].main,"Q",e.units[r].controllers,n),Iu(n,e.units[r],"Q")}}}function dd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].resume&&(e.units[r].resume.buffer=n)}}}function bd(t){return function(e){return function(){var r=t.id,n=t.buffer;e.units[r].main.buffer=n}}}function yd(t){return function(e){return function(){var r=t.id,n=t.spec;e.units[r].resume&&(e.units[r].resume.spec=n)}}}function Ad(t){return function(e){return function(){var r=t.id,n=t.pan;Ou(e,e.units[r].main,"pan",e.units[r].controllers,n),Iu(n,e.units[r],"pan")}}}function kd(t){return function(e){return function(){var r=t.id,n=t.threshold;Ou(e,e.units[r].main,"threshold",e.units[r].controllers,n),Iu(n,e.units[r],"threshold")}}}function gd(t){return function(e){return function(){var r=t.id,n=t.loopStart;e.units[r].main.loopStart=n,e.units[r].resume.loopStart=n}}}function Ed(t){return function(e){return function(){var r=t.id,n=t.loopEnd;e.units[r].main.loopEnd=n,e.units[r].resume.loopEnd=n}}}function Cd(t){return function(e){return function(){var r=t.id,n=t.bufferOffset;e.units[r].resume.bufferOffset=n}}}function Sd(t){return function(e){return function(){var r=t.id,n=t.duration;e.units[r].duration=n}}}function hd(t){return function(e){return function(){var r=t.id,n=t.release;Ou(e,e.units[r].main,"release",e.units[r].controllers,n),Iu(n,e.units[r],"release")}}}function Td(t){return function(e){return function(){var r=t.id,n=t.offset;Ou(e,e.units[r].main,"offset",e.units[r].controllers,n),Iu(n,e.units[r],"offset")}}}function xd(t){return function(e){return function(){var r=t.id,n=t.ratio;Ou(e,e.units[r].main,"ratio",e.units[r].controllers,n),Iu(n,e.units[r],"ratio")}}}function Fd(t){return function(e){return function(){var r=t.id,n=t.attack;Ou(e,e.units[r].main,"attack",e.units[r].controllers,n),Iu(n,e.units[r],"attack")}}}function $d(t){return function(e){return function(){var r=t.id,n=t.knee;Ou(e,e.units[r].main,"knee",e.units[r].controllers,n),Iu(n,e.units[r],"knee")}}}function wd(t){return function(e){return function(){var r=t.id,n=t.delayTime;Ou(e,e.units[r].main,"delayTime",e.units[r].controllers,n),Iu(n,e.units[r],"delayTime")}}}function Pd(t){return function(e){return function(){var r=t.id,n=t.playbackRate;Ou(e,e.units[r].main,"playbackRate",e.units[r].controllers,n),Iu(n,e.units[r],"playbackRate")}}}function Md(t){return function(e){return function(){var r=t.id,n=t.frequency;Ou(e,e.units[r].main,"frequency",e.units[r].controllers,n),Iu(n,e.units[r],"frequency")}}}function Od(t){return function(e){return function(){var r=t.id,n=t.onOff;n.x.type==="on"?FO(r)(n)(e)():n.x.type==="off"&&$O(r)(n)(e)()}}}var FO=function(t){return function(e){return function(r){return function(){if(!r.units[t].onOff){r.units[t].pendingOn=!1,r.units[t].onOff=!0,r.units[t].main=r.units[t].createClosure(r.context,r.units[t].resume);for(var n=0;n<r.units[t].audioOutgoing.length;n++){var u=r.units[t].audioOutgoing[n];r.units[t].main.connect(r.units[u].main),r.units[u].se&&r.units[t].main.connect(r.units[u].se)}r.units[t].resume&&r.units[t].resume.bufferOffset?r.units[t].resume.duration.type==="just"?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset,r.units[t].resume.duration.value):r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.bufferOffset):r.units[t].resume&&r.units[t].resume.loopStart?r.units[t].main.start(r.deprecatedWriteHead+e.o,r.units[t].resume.loopStart):r.units[t].main.start(r.deprecatedWriteHead+e.o)}}}}},$O=function(t){return function(e){return function(r){return function(){if(!!r.units[t].onOff){r.units[t].onOff=!1;var n=r.units[t].main;n.addEventListener("ended",()=>{n.disconnect()}),n.stop(r.deprecatedWriteHead+e.o)}}}}};function Id(t){return function(){for(var e=new Float32Array(t.length),r=0;r<t.length;r++)e[r]=t[r];return e}}function ps(t){return function(){t.stop()}}function Rd(t){return function(e){return function(r){return function(){var n=[];r.ondataavailable=function(u){n.push(u.data)},r.onstop=function(){var u=new Blob(n,{type:t});e(u)(),n=null}}}}}function Nd(t){return function(e){return function(){return navigator.mediaDevices.getUserMedia({audio:t,video:e})}}}function S_(t){return function(){var e=new Uint8Array(t.frequencyBinCount);return t.getByteFrequencyData(e),e}}function Ld(t){return function(){var e=t.createConstantSource();return e.offset.value=0,e.connect(t.destination),e.start(),function(){e.stop(),e.disconnect(t.destination)}}}var Wd=function(t){return function(e){return function(r){return function(){for(var n=new Float32Array(e.length),u=new Float32Array(r.length),o=0;o<e.length;o++)n[o]=e[o];for(var o=0;o<r.length;o++)u[o]=r[o];return t.createPeriodicWave(n,u,{disableNormalization:!0})}}}};function Ki(t){return function(){return{context:t,deprecatedWriteHead:0,units:{},unqidfr:bO(10),scopes:{},unsu:{},toConnect:{}}}}function qd(t){return function(){t.close()}}function Gd(t){return function(){return fetch(t).then(function(e){return e.arrayBuffer()},function(e){return console.error("Error fetching buffer",e),Promise.reject(e)})}}function Bd(t){return function(e){return function(){return t.decodeAudioData(e)}}}function Ud(){return new(window.AudioContext||window.webkitAudioContext)}function Hd(t){return function(){return t.state}}function h_(t){return function(){return t.currentTime}}function OC(t){return function(e){return function(r){return function(){t.then(r,e)}}}}var MO=function(t){return function(e){return Oc(function(r){return I_(C)(De(TC))(OC(e)(function(n){return r(Mt.create(t(n)))()})(function(n){return r(Ot.create(n))()}))})}};var OO=function(t){return Na(function(e){return Mo("Promise failed, couldn't extract JS Error or String")})(Y(Z))(Bv(P(Gv(xm)(zu))(Xv(zu)("Error")(t))(_(n_(Eo))(Mo)(Qv(zu)(t)))))},IC=MO(OO),ss=function(t){return nt(Ir)(vr(Or)(t))(IC)};function zd(t){return function(){return URL.createObjectURL(t)}}var RC=function(t){return function(e){return function(r){return ht(Rd(t))(r)(function(){var n=zn(Ge)(e);return function(u){return n(zd(u))}}())}}};var Wf={ids:_(C)(jt(Xf))(Ka),deleteFromCache:MD,disconnectXFromY:ID,connectXToY:OD,makeAllpass:RD,makeAnalyser:ND,makeAudioWorkletNode:LD,makeBandpass:WD,makeConstant:qD,makeConvolver:GD,makeDelay:BD,makeDynamicsCompressor:UD,makeGain:HD,makeHighpass:zD,makeHighshelf:VD,makeIIRFilter:JD,makeLoopBuf:jD,makeLowpass:XD,makeLowshelf:QD,makeMediaElement:KD,makeMicrophone:YD,makeNotch:ZD,makePeaking:td,makePeriodicOsc:ed,makePlayBuf:rd,makeRecorder:nd,makeSawtoothOsc:ad,makeSinOsc:ud,makeSpeaker:od,setDuration:Sd,makeSquareOsc:fd,makeStereoPanner:id,makeTriangleOsc:cd,makeWaveShaper:ld,setAnalyserNodeCb:_d,setMediaRecorderCb:pd,setWaveShaperCurve:sd,setAudioWorkletParameter:md,setBuffer:dd,setConvolverBuffer:bd,setPeriodicOsc:yd,setOnOff:Od,setBufferOffset:Cd,setLoopStart:gd,setLoopEnd:Ed,setRatio:xd,setOffset:Td,setAttack:Fd,setGain:vd,setQ:Dd,setPan:Ad,setThreshold:kd,setRelease:hd,setKnee:$d,setDelay:wd,setPlaybackRate:Pd,setFrequency:Md},Et=function(t){return function(e){return nt(Ir)(ss(Gd(e)))(function(){var r=Bd(t);return function(n){return ss(r(n))}}())}},T_=function(t){var e=vr(t);return function(r){return e(Hd(r))}};var Kn=function(t){return vr(t)(Ud)},Ru=function(t){var e=vr(t);return function(r){return e(Ld(r))}},kn=function(t){return function(e){return vr(t)(function(){var n=T_(fr)(e)();return En(tt)(n!=="closed")(qd(e))()})}},WO=_t,qO=_t,ms=function(t){return function(e){return _(Di)(function(r){return{microphone:function(){return t?L(go)(WO(r)):z.value}(),camera:function(){return e?L(go)(qO(r)):z.value}()}})(ss(Nd(t)(e)))}};var Wo=function(){function t(){}return t.value=new t,t}(),qo=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),Au=function(){function t(){}return t.value=new t,t}(),_n=FD,bi=function(t){return mo($n)(P(xC)(Fn($n)(nt(Ir)(SC(t))(vr(Or))))(Fn($n)(hC(Mo("We navigated away from the page"))(t))))},Lc=function(t){return function(e){return function(r){return function(n){return P(t)(K(e)(Au.value))(n)}}}},Oa=function(t){return function(e){return function(r){return function(n){return P(t)(K(e)(Q(dr)(sr.value)(rr(S(n)))))(_(t.Functor0())(function(u){return Q(dr)(sr.value)(rr(S(H(J)(u)(n))))})(_(t.Functor0())(function(u){return u.value0})(r)))}}}},vs=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return function(l){return _(t)(function(p){return Q(dr)(sr.value)(rr(S(function(){if(p.value0 instanceof Wo)return L(tt)(void 0);if(p.value0 instanceof qo)return H(J)(H(J)(p.value0.value0)(n(L(tt)(void 0))))(u(Au.value));if(p.value0 instanceof Au)return function(){p.value1(),u(Wo.value)();var m=vo(nt(Ir)(Kn(Or))(function(v){return nt(Ir)(Ru(Or)(v))(function(f){return nt(Ir)(o(v))(function(b){return vr(Or)(function(){var vt=c(v)(b)(),Qt=H(J)(H(J)(vt)(f))(kn(fr)(v));return u(new qo(Qt))(),Qt})})})}))();return Vf(e)(Ge)(n(function(){return u(Au.value)(),Lo(bi(m))()}))(function(){return L(tt)(void 0)})()};throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 48, column 21 - line 66, column 26): "+[p.value0.constructor.name])}())))})(Tn(r)(P(r.Plus0().Alt0())(K(r)(L(tt)(void 0)))(_(t)(function(p){return p.value0})(i)))(_(t)(rt.create)(l)))}}}}}}}}},Yn=function(t){return function(e){return function(r){return function(){return t(r)(),e(new FC(r))()}}}},Ds=function(t){return function(e){return function(r){return function(n){return function(u){return an(function(o){return function(c){var i=Lc(R)(E)(e)(c);return F(N(a)(a))(vc($e(a)(a))(P(R)(K(E)(Q(Up)(Ht.value)("cursor: pointer;")))(vs(A)(Jo)(E)(r)(o)(n)(u)(e)(i)))([un(_(A)(function(l){if(l instanceof Au)return t;if(l instanceof Wo)return"\u23F3";if(l instanceof qo)return"\u{1F6D1}";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 115, column 19 - line 118, column 37): "+[l.constructor.name])})(i))]))}})}}}}},xt=function(t){return function(e){return function(r){return function(n){return an(function(u){return function(o){var c=Lc(R)(E)(t)(o);return F(N(a)(a))(An($e(a)(a))(vs(A)(Jo)(E)(e)(u)(r)(n)(t)(c))([un(_(A)(function(i){if(i instanceof Au)return"Turn on";if(i instanceof Wo)return"Loading...";if(i instanceof qo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.Util (line 89, column 19 - line 92, column 44): "+[i.constructor.name])})(c))]))}})}}}};var Wc=function(t){return function(e){return function(){var n=Ki(t)(),u=gt(If(aE(a)(a)(a))(_(A)(Ke(W(a)(a)(a)))(e))(Wf))(function(o){return o(n)})();return u}}};var st=function(t){return function(e){return function(){var n=Ki(t)(),u=gt(If(W(a)(a)(a))(e)(Wf))(function(o){return o(n)})();return u}}},ds=function(t){return function(){var r=Kn(fr)();return _(C)(function(n){return H(J)(n)(kn(fr)(r))})(st(r)(t))()}};var GO=function(){return D.value}(),NC=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(D.value)(GO)({allpass:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Pa(ue(Ce)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(.2)([o,m_(gD)(W(a)(a)(a))(700)([m_(ns(kt(At()(U(U(yt)(cD)()()()({reflectSymbol:function(){return"q"}}))(es)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:990,q:20})([o]),m_(gD)(W(a)(a)(a))(1110)([o,m_(ns(kt(At()(U(U(yt)(cD)()()()({reflectSymbol:function(){return"q"}}))(es)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:2010,q:30})([o])])])]))}})])}}))})}}};function yi(t){return function(r,n,u){if(n===null)return new t(r);var o=r.byteLength,c=t.BYTES_PER_ELEMENT,i=Math.min(o,n>>>0);if(u===null)return new t(r,i);var l=Math.min((o-i)/c,u);return new t(r,i,l)}}var UO=yi(Uint8ClampedArray),HO=yi(Uint32Array),zO=yi(Uint16Array),LC=yi(Uint8Array),VO=yi(Int32Array),JO=yi(Int16Array),jO=yi(Int8Array),XO=yi(Float32Array),QO=yi(Float64Array);function WC(t){for(var e=t.length,r=new Array(e),n=0;n<e;n++)r[n]=t[n];return r}var bs={create:LC,BinaryValue0:function(){}};var ys=function(t){return function(e){return function(){return WC(e)}}};function As(t){return t|0}var Ai=function(){return window};function UC(t,e,r,n){if(typeof window<"u"){var u=window[r];if(u!=null&&n instanceof u)return e(n)}for(var o=n;o!=null;){var c=Object.getPrototypeOf(o),i=c.constructor.name;if(i===r)return e(n);if(i==="Object")return t;o=c}return t}var Nt=function(t){return function(e){return UC(z.value,B.create,t,e)}};function HC(t){return function(){return t.body}}var zC=function(){var t=_(C)(Xr);return function(e){return t(HC(e))}}();var VC=_t;function qf(t){return function(){return t.valueAsNumber}}var qc=Nt("HTMLInputElement");function Jd(t){return function(){return t.document}}function ks(t){return function(e){return function(){return e.requestAnimationFrame(t)}}}var jd=_t;var II=function(t,e,r){var n=0,u;return function(o){if(n===2)return u;if(n===1)throw new ReferenceError(t+" was needed before it finished initializing (module "+e+", line "+o+")",e,o);return n=1,u=r(),n=2,u}},Yi=It(function(t){return function(){var r=Ai(),n=ge(!0)(),u=II("fx","FRP.Event.Animate",function(){return Le(C)(ht(ks)(r)(function(){var i=Me(n)();return En(tt)(i)(function(){return t(void 0)(),u(19)()})()}))}),o=u(15);return o(),je(!1)(n)}});var RI="background-color: rgb(150,30,10);",NI="background-color: rgb(130,60,10);",LI="background-color: rgb(80,90,10);",WI="background-color: rgb(10,130,10);",qI="background-color: rgb(10,100,0);",GI=Op(Qa)(function(t){return ke(de(Ep)()(Ua)()(Bl))(RI)(ke(de(Qn)()(Gr)()(Ua))(NI)(ke(de(xa)()(Mr)()(Gr))(LI)(ke(de(Fa)()(xr)()(Mr))(WI)(ke(de(ia)()(sa)()(xr))(qI)($a)))))}),BI=function(t){return function(e){return function(r){return function(n){return is(os(kt(At()(U(U(yt)(us)()()()({reflectSymbol:function(){return"fftSize"}}))(t)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))(Qe(a)(a)(a))({cb:n,fftSize:Xp.value})(ue(e)(r)(lt()))}}}},UI=function(){return D.value}(),Re="background-color: rgb(255,255,255,0.0);",qe=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return function(l){return function(p){return function(s){return _(t)(function(m){var v=hf(e)()(hf(n)()(m)(l))(p);return v?Q(o)(Ht.value)(hf(e)()(hf(n)()(GI)(l))(p)):Q(o)(Ht.value)(Re)})(s)}}}}}}}}}}},HI=function(){return 15/40}(),zI=function(){return 10/40}(),VI=function(){return 7/40}(),JI=function(){return 3/40}(),jI=function(){return 1/40}(),jC=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="analyser">Analyser</h2>
  <p>An <a href="https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode">analyser node</a> provides methods to recuperate the analysed data of an input. This is how, for example, Google Meet shows the little animation around a microphone icon. Wags provides the possibility to use the analyser as the terminus of an audio graph <i>or</i> as part of a longer DSP chain, as in the following example. The example uses an FFT size of 256, which is indicated in Wags as <code>TTT8</code> (two to the eighth power).</p>

  <pre><code>analyser_ { cb, fftSize: TTT8 } (loopBuf atar bangOn)</code></pre>

  ~analyser~
  </section>
`}})()()(M()(X)({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))(UI)({analyser:O(et(a)(a))(an(function(n){return function(u){var o=Fl(du)(Y(Z))(u),c=Lc(R)(E)(r)(function(l){return l.right}(o)),i=function(l){return l.left}(o);return F(N(a)(a))(Ue($e(a)(a))([An($e(a)(a))(P(R)(K(E)(Q(sc)(Ht.value)("cursor: pointer;")))(vs(A)(Jo)(E)(t)(function(l){return n(Ot.create(l))})(function(l){return Et(l)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(l){return function(p){return function(){var m=ge(z.value)(),v=Ki(l)(),f=If(W(a)(a)(a))([BI(as)(Ce)(p)(function(ct){return function(){return je(new B(ct))(m)(),je(z.value)(m)}})])(Wf),b=gt(P(R)(_(A)(Ot.create)(f))(_(A)(Mt.create)(Yi)))(function(ct){if(ct instanceof Ot)return ct.value0(v);if(ct instanceof Mt)return function(){var Qt=Me(m)();return aa(tt)(ur)(Qt)(function(ee){return function(){var Hr=S_(ee)(),it=ys(bs)(Hr)(),oe=ge(0)(),be=ge(0)(),qt=ge(0)(),kr=ge(0)(),Rr=ge(0)(),da=ge(0)(),Un=ge(0)(),ba=ge(0)(),pn=ge(0)(),Bo=ge(0)(),Bf=function(Fr){if(Fr<32)return oe;if(Fr<64)return be;if(Fr<96)return qt;if(Fr<128)return kr;if(Fr<168)return Rr;if(Fr<160)return da;if(Fr<224)return Un;if(nr)return ba;throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 143, column 45 - line 151, column 63): "+[Fr.constructor.name])};Mi(it)(function(Fr){var Uo=As(Fr);return function(){var Ho=Me(Bo)();return na(Ve(jo)(Uo))(pn)(),na(Ve(jo)(Uo))(Bf(Ho))(),na(Ve(jo)(1))(Bo)()}})();var Uf=Jn(Ck)(tt)(function(Fr){return function(){var Hf=_(C)(Xe)(Me(Fr))(),Ho=_(C)(Vu(ll)(Hf))(_(C)(Xe)(Me(pn)))();return ke(de(Ep)()(Ua)()(Bl))(Ho>HI)(ke(de(Qn)()(Gr)()(Ua))(Ho>zI)(ke(de(xa)()(Mr)()(Gr))(Ho>VI)(ke(de(Fa)()(xr)()(Mr))(Ho>JI)(ke(de(ia)()(sa)()(xr))(Ho>jI)($a)))))}})(ke(de(nk)()(ov)()(ok))(oe)(ke(de(ak)()(iv)()(ov))(be)(ke(de(uk)()(Bl)()(iv))(qt)(ke(de(Ep)()(Ua)()(Bl))(kr)(ke(de(Qn)()(Gr)()(Ua))(Rr)(ke(de(xa)()(Mr)()(Gr))(da)(ke(de(Fa)()(xr)()(Mr))(Un)(ke(de(ia)()(sa)()(xr))(ba)($a)))))))))();return n(new Mt(Uf))()}})()};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 124, column 57 - line 162, column 57): "+[ct.constructor.name])})();return function(){return b(),function(){var Qt=T_(fr)(l)();return En(tt)(Qt!=="closed")(kn(fr)(l))()}(),n(new Mt(Op(Qa)(S(Op(Ta)(S(!1))))))()}}}})(r)(c)))([un(_(A)(function(l){if(l instanceof Au)return"Turn on";if(l instanceof Wo)return"Loading...";if(l instanceof qo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Analyser (line 175, column 31 - line 178, column 56): "+[l.constructor.name])})(c))]),Fe($e(a)(a))(K(E)(Q(pt)(Ht.value)("display: grid; grid-template-columns: repeat(8, 1fr); grid-auto-rows: 20px;")))([Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(yr)(io)(pt)(pa)(io)(wa)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Wn)(oo)(pt)(pa)(oo)(ru)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Ln)(uo)(pt)(pa)(uo)(eu)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Nn)(ao)(pt)(pa)(ao)(tu)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Rn)(no)(pt)(pa)(no)(Za)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Ta)(ro)(pt)(pa)(ro)(Fc)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(Po)(eo)(pt)(pa)(eo)(xc)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(yr)(pa)(wo)(to)(pt)(pa)(to)(Tc)(wa)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(yr)(io)(pt)(_a)(io)(wa)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Wn)(oo)(pt)(_a)(oo)(ru)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Ln)(uo)(pt)(_a)(uo)(eu)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Nn)(ao)(pt)(_a)(ao)(tu)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Rn)(no)(pt)(_a)(no)(Za)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Ta)(ro)(pt)(_a)(ro)(Fc)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(Po)(eo)(pt)(_a)(eo)(xc)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Wn)(_a)(wo)(to)(pt)(_a)(to)(Tc)(ru)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(yr)(io)(pt)(la)(io)(wa)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Wn)(oo)(pt)(la)(oo)(ru)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Ln)(uo)(pt)(la)(uo)(eu)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Nn)(ao)(pt)(la)(ao)(tu)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Rn)(no)(pt)(la)(no)(Za)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Ta)(ro)(pt)(la)(ro)(Fc)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(Po)(eo)(pt)(la)(eo)(xc)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Ln)(la)(wo)(to)(pt)(la)(to)(Tc)(eu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(yr)(io)(pt)(ca)(io)(wa)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Wn)(oo)(pt)(ca)(oo)(ru)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Ln)(uo)(pt)(ca)(uo)(eu)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Nn)(ao)(pt)(ca)(ao)(tu)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Rn)(no)(pt)(ca)(no)(Za)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Ta)(ro)(pt)(ca)(ro)(Fc)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(Po)(eo)(pt)(ca)(eo)(xc)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Nn)(ca)(wo)(to)(pt)(ca)(to)(Tc)(tu)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(yr)(io)(pt)(fa)(io)(wa)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Wn)(oo)(pt)(fa)(oo)(ru)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Ln)(uo)(pt)(fa)(uo)(eu)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Nn)(ao)(pt)(fa)(ao)(tu)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Rn)(no)(pt)(fa)(no)(Za)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Ta)(ro)(pt)(fa)(ro)(Fc)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(Po)(eo)(pt)(fa)(eo)(xc)(Za)(i)))(ae),Fe(N(a)(a))(P(R)(K(E)(Q(pt)(Ht.value)(Re)))(qe(A)(Rn)(fa)(wo)(to)(pt)(fa)(to)(Tc)(Za)(i)))(ae)])]))}}))})}}};var QI=function(){return D.value}(),XC=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))(D.value)(QI)({bandpass:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Pa(ue(Ce)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(.8)([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:400,q:1})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:880,q:5})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:1200,q:10})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:2e3,q:20})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:3e3,q:30})([o])]))}})])}}))})}}};var YI=function(){return D.value}(),QC=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))(YI)({compression:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([nC(jE(kt(At()(yt))(bt()())))(W(a)(a)(a))({})([ue(Ce)(u)(lt())])])}}))})}}};var Zn=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"playbackRate"}})(D.value),n=Pf(t);return function(u){return e(r(n(u)))}}},Gf=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"onOff"}})(D.value),n=uE(t);return function(u){return e(r(n(u)))}}},KC=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"offset"}})(D.value),n=Pf(t);return function(u){return e(r(n(u)))}}},YC=function(){var t=en(),e=He()({reflectSymbol:function(){return"loopStart"}})(D.value);return function(r){return t(e(r))}},ZC=function(){var t=en(),e=He()({reflectSymbol:function(){return"loopEnd"}})(D.value);return function(r){return t(e(r))}},gn=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"gain"}})(D.value),n=Pf(t);return function(u){return e(r(n(u)))}}},Do=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"frequency"}})(D.value),n=Pf(t);return function(u){return e(r(n(u)))}}};var Gc=function(){return function(t){var e=en(),r=He()({reflectSymbol:function(){return"delayTime"}})(D.value),n=Pf(t);return function(u){return e(r(n(u)))}}};var tR=function(){return D.value}(),tS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="constant">Constant value</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConstantSourceNode">Constant values</a>, or DC offset, is a way to output an unchanging stream of values. This is only really useful when testing the performance of speakers or microphones and/or when working with a custom audio node that supports constant streaming values. Note that the constant source node in the web audio API can <i>also</i> be used to control audio parameters. Wags uses this feature of constant nodes under the hood to optimize certain computations.</p>

  <p>The following example abuses a constant audio node by turning it into a gnarly inpulse generator. We'll learn about the tie fighter symbol <code>~tf~</code> and the <code>bang</code> in the next section on Events. Kids, don't try this at home!</p>

  <pre><code>~txt~</code></pre>

  ~constant~
  </section>
`}})()()(M()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"tf"}})({reflectSymbol:function(){return"tf"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))(tR)({tf:O(N(a)(a))(cr("<|>")),txt:O(N(a)(a))(cr(`run2_
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
  ]`)),constant:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.5)([cs(ts)(0)(P(R)(lt())(K(E)(KC()(xn)({d:5,o:.1,p:Xu(mf)(function(o){return S(function(){var c=iu(Co)(o)(3)===0;return c?1:0}())})(sn(0)(1920))}))))])])}}))})}}};var rR=function(){return D.value}(),eS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="convolution">Convolution</h2>
  <p><a href="https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode">Convolution</a>, aka reverb, is a way to graft the shape of one sound (usually an <a href="https://en.wikipedia.org/wiki/Impulse_response">impulse response</a>) onto another. Convolution can sound great, but it is a <i>very expensive operation</i> that will cause noticeable artifacts on low-end devices. When shipping audio code to production, you're usually better off using an Audio Worklet Node with reverb optimized for your specific case. That said, for PoCs or hobbyist projects, convolution is great!</p>

  <pre><code>\\{loop, verb} -> run2_
  [ convolver verb [ loopBuf loop bangOn ] ]</code></pre>

  ~convolution~
  </section>
`}})()()(M()(X)({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))(rR)({convolution:O(et(a)(a))(xt(r)(t)(function(n){return Vt(EC)(_(Di)(function(u){return function(o){return{loop:u,verb:o}}})(Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")))(Et(n)("https://cdn.jsdelivr.net/gh/andibrae/Reverb.js/Library/StMarysAbbeyReconstructionPhase3.m4a"))})(function(n){return function(u){return st(n)([aC(dE)(W(a)(a)(a))(u.verb)([ue(Ce)(u.loop)(lt())])])}}))})}}};var aR=function(){return D.value}(),rS=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))(D.value)(aR)({delay:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return st(n)([Pa(Gn(Va)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(.2)([so(Qr)(W(a)(a)(a))(.03)([o]),so(Qr)(W(a)(a)(a))(.1)([o]),so(Qr)(W(a)(a)(a))(.3)([o]),so(Qr)(W(a)(a)(a))(.7)([o])]))}})])}}))})}}};var oR=function(){return D.value}(),nS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))(oR)({gain:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.1)([ue(Ce)(u)(lt())])])}}))})}}};var fR=function(){return D.value}(),aS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highpass">Highpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highpass filter</a> lets higher frequencies pass and amortizes lower ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ highpass_ 2000.0
      [ loopBuf buf bangOn ]
  ]
</code></pre>

  ~highpass~
  </section>
`}})()()(M()(X)({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))(fR)({highpass:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([$c(Ya)(W(a)(a)(a))(2e3)([ue(Ce)(u)(lt())])])}}))})}}};var lR=function(){return D.value}(),uS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="highshelf">Highshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">highshelf filter</a> boosts or attenuates high frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
  [ highshelf_ { frequency: 2000.0, gain: 0. }
      [ loopBuf buf bangOn ]
  ]</code></pre>

  ~highshelf~
  </section>
`}})()()(M()(X)({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))(lR)({highshelf:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([ZE(JE(kt(At()(U(U(yt)(CE)()()()({reflectSymbol:function(){return"gain"}}))(SE)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:2e3,gain:.4})([ue(Ce)(u)(lt())])])}}))})}}};var pR=function(){return D.value}(),oS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"iirFilterEx"}})({reflectSymbol:function(){return"iirFilterEx"}}))(pR)({iirFilterEx:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([KE()()(W(a)(a)(a))(DE(a)(a))(W(a)(a)(a))(new rt(ke(de(xa)()(Mr)()(Gr))(20298e-8)(ke(de(Fa)()(xr)()(Mr))(.0004059599)(ke(de(ia)()(sa)()(xr))(20298e-8)($a))),ke(de(xa)()(Mr)()(Gr))(1.0126964558)(ke(de(Fa)()(xr)()(Mr))(-1.9991880801)(ke(de(ia)()(sa)()(xr))(.9873035442)($a)))))([ue(Ce)(u)(lt())])])}}))})}}};var mR=function(){return D.value}(),iS=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))(D.value)(mR)({loopBuf:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/100/100981_1234256-lq.mp3")})(function(n){return function(u){return st(n)([ue(Of(kt(At()(U(U(U(U(yt)(Cc)()()()({reflectSymbol:function(){return"playbackRate"}}))(__)()()()({reflectSymbol:function(){return"loopStart"}}))(l_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Mf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:u,playbackRate:.5,loopStart:.1,loopEnd:.6})(lt()),ue(Of(kt(At()(U(U(U(U(yt)(Cc)()()()({reflectSymbol:function(){return"playbackRate"}}))(__)()()()({reflectSymbol:function(){return"loopStart"}}))(l_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Mf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:u,playbackRate:1,loopStart:.5,loopEnd:1.2})(lt()),ue(Of(kt(At()(U(U(yt)(Cc)()()()({reflectSymbol:function(){return"playbackRate"}}))(Mf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:u,playbackRate:1.7})(lt())])}}))})}}};var DR=function(){return D.value}(),fS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowpass">Lowpass filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowpass filter</a> lets lower frequencies pass and amortizes higher ones. If you amp up the Q value, the effect will be sharper.</p>

  <pre><code>\\buf -> run2_
  [ lowpass_ 215.0 [ loopBuf buf bangOn ] ]
</code></pre>

  ~lowpass~
  </section>
`}})()()(M()(X)({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))(DR)({lowpass:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([wc(rs)(W(a)(a)(a))(215)([ue(Ce)(u)(lt())])])}}))})}}};var bR=function(){return D.value}(),cS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="lowshelf">Lowshelf filter</h2>
  <p>A <a href="https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode">lowshelf filter</a> boosts or attenuates lower frequencies using a <code>gain</code> parameter, where gain is expressed in decibels from -40.0 to 40.0.</p>

  <pre><code>\\buf -> run2_
   [ lowshelf_ { frequency: 91.0, gain: 10.0 }
       [ loopBuf buf bangOn ]
   ]
</code></pre>

  ~lowshelf~
  </section>
`}})()()(M()(X)({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))(bR)({lowshelf:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([eC(VE(kt(At()(U(U(yt)(kE)()()()({reflectSymbol:function(){return"gain"}}))(gE)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:91,gain:.4})([ue(Ce)(u)(lt())])])}}))})}}};var AR=function(){return D.value}(),lS=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))(D.value)(AR)({microphone:O(et(a)(a))(xt(r)(t)(function(n){return ms(!0)(!1)})(function(n){return function(u){return st(n)([function(){if(u.microphone instanceof B)return nu(function(o){return Rt(ot)(W(a)(a)(a))(1)([D_(f_)(u.microphone.value0),so(Qr)(W(a)(a)(a))(.1)([Rt(ot)(W(a)(a)(a))(.2)([o])])])});if(u.microphone instanceof z)return Rt(ot)(W(a)(a)(a))(.02)([oC(Qi)(440)]);throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Microphone (line 43, column 15 - line 48, column 56): "+[u.microphone.constructor.name])}()])}}))})}}};var gR=function(){return D.value}(),_S=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))(gR)({notch:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Pc(hc(kt(At()(U(U(yt)(gc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:400,q:1})(Pc(hc(kt(At()(U(U(yt)(gc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:880,q:5})(Pc(hc(kt(At()(U(U(yt)(gc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:1200,q:10})(Pc(hc(kt(At()(U(U(yt)(gc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:2e3,q:20})(Pc(hc(kt(At()(U(U(yt)(gc)()()()({reflectSymbol:function(){return"q"}}))(Ec)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:3e3,q:30})(ue(Ce)(u)(lt()))))))])}}))})}}};var CR=function(){return D.value}(),pS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))(CR)({peaking:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Mc(Sc(kt(At()(U(U(U(yt)(yc)()()()({reflectSymbol:function(){return"q"}}))(Ac)()()()({reflectSymbol:function(){return"gain"}}))(kc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:400,q:1,gain:-20})(Mc(Sc(kt(At()(U(U(U(yt)(yc)()()()({reflectSymbol:function(){return"q"}}))(Ac)()()()({reflectSymbol:function(){return"gain"}}))(kc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:880,q:5,gain:20})(Mc(Sc(kt(At()(U(U(U(yt)(yc)()()()({reflectSymbol:function(){return"q"}}))(Ac)()()()({reflectSymbol:function(){return"gain"}}))(kc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:1200,q:10,gain:-20})(Mc(Sc(kt(At()(U(U(U(yt)(yc)()()()({reflectSymbol:function(){return"q"}}))(Ac)()()()({reflectSymbol:function(){return"gain"}}))(kc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:2e3,q:20,gain:20})(Mc(Sc(kt(At()(U(U(U(yt)(yc)()()()({reflectSymbol:function(){return"q"}}))(Ac)()()()({reflectSymbol:function(){return"gain"}}))(kc)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(Qe(a)(a)(a))({frequency:3e3,q:30,gain:-20})(ue(Ce)(u)(lt()))))))])}}))})}}};var hR=function(){return D.value}(),sS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(hR)({periodic:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.2)([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:140,spec:new rt(ke(de(Qn)()(Gr)()(Ua))(.1)(ke(de(xa)()(Mr)()(Gr))(.2)(ke(de(Fa)()(xr)()(Mr))(.3)(ke(de(ia)()(sa)()(xr))(.4)($a)))),ke(de(Qn)()(Gr)()(Ua))(.4)(ke(de(xa)()(Mr)()(Gr))(.3)(ke(de(Fa)()(xr)()(Mr))(.2)(ke(de(ia)()(sa)()(xr))(.1)($a)))))})(lt())])])}}))})}}};var xR=function(){return D.value}(),mS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(X)({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))(xR)({playBuf:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/470/470035_9564355-lq.mp3")})(function(n){return function(u){return st(n)([Gn(p_(kt(At()(U(U(U(yt)(yE)()()()({reflectSymbol:function(){return"duration"}}))(bE)()()()({reflectSymbol:function(){return"bufferOffset"}}))(c_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:u,duration:3,bufferOffset:4.2})(lt())])}}))})}}};var Xd=function(){function t(){}return t.value=new t,t}();var vS={attr:function(t){return function(e){return d({key:"controls",value:q(e)})}}};var Qd=function(){function t(){}return t.value=new t,t}();var DS={attr:function(t){return function(e){return d({key:"src",value:q(e)})}}};var Kd=function(t){return function(e){return function(r){return V("audio")(e)(F(t)(r))}}};var MR=function(t){return function(e){return function(r){return function(n){return hD(t)(n)(D_(e)(r))}}}},OR=function(){return D.value}(),dS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="recorder">Recorder</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode">recorder</a> allows you to record audio. It takes a callback that you can use to stash the recorded audio somewhere, like in a file for example, as the example below does. You can use it as a simple note-taking app \u{1F399}\uFE0F.</p>

  <pre><code>\\cb m -> recorder cb (microphone m)</code></pre>

  ~recorder~
  </section>
`}})()()(M()(X)({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))(OR)({recorder:O(et(a)(a))(an(function(n){return function(u){var o=Fl(du)(Y(Z))(u),c=Fl(du)(Y(Z))(function(s){return s.left}(o)),i=function(s){return s.right}(c),l=Lc(R)(E)(r)(function(s){return s.right}(o)),p=function(s){return s.left}(c);return F(N(a)(a))(Ue($e(a)(a))([An($e(a)(a))(P(R)(K(E)(Q(sc)(Ht.value)("cursor: pointer;")))(_(A)(function(s){return Q(dr)(sr.value)(rr(S(function(){if(s.e instanceof Wo)return L(tt)(void 0);if(s.e instanceof qo)return H(J)(H(J)(H(J)(s.e.value0)(t(L(tt)(void 0))))(aa(tt)(ur)(s.rec)(function(){var m=Hl(cv);return function(v){return m(ps(v))}}())))(n(Ot.create(Au.value)));if(s.e instanceof Au)return function(){s.cncl();var v=Du();n(new Ot(Wo.value))();var f=vo(nt(Ir)(_(Di)(function(b){return b.microphone})(ms(!0)(!1)))(function(b){return vr(Or)(function(){var vt=jr(L(tt)(L(tt)(void 0)))(function(Qt){return function(){var xe=Kn(fr)(),Hr=Ki(xe)(),it=If(W(a)(a)(a))([MR(uD)(f_)(Qt)(function(be){return function(){return n(new Mt(new Ot(be)))(),Le(C)(wu(be)(v))(),RC("audio/ogg; codecs=opus")(function(kr){return n(Mt.create(Mt.create(kr)))})(be)()}})])(Wf),oe=gt(it)(function(be){return be(Hr)})();return function(){oe(),nt(Ge)(Mk(v))(mr(tt)(ur)(function(){var kr=Hl(cv);return function(Rr){return kr(ps(Rr))}}()))();var qt=T_(fr)(xe)();return En(tt)(qt!=="closed")(kn(fr)(xe))()}}})(b)();return n(new Ot(new qo(vt)))(),vt})}))();return t(function(){return n(Ot.create(Au.value))(),Lo(bi(f))()})(),void 0};throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 63, column 47 - line 106, column 52): "+[s.e.constructor.name])}())))})(Tn(E)(P(R)(K(E)(z.value))(_(A)(B.create)(i)))(_(A)(tl)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(s){return s.value0})(r)))(_(A)(function(s){return function(m){return function(v){return{e:s,cncl:m,rec:v}}}})(l)))))))([un(_(A)(function(s){if(s instanceof Au)return"Turn on";if(s instanceof Wo)return"Loading...";if(s instanceof qo)return"Turn off";throw new Error("Failed pattern match at WAGS.Example.Docs.AudioUnits.Recorder (line 117, column 31 - line 120, column 56): "+[s.constructor.name])})(l))]),Ue($e(a)(a))([Kd(N(a)(a))(P(R)(K(E)(Q(vS)(Xd.value)("true")))(P(R)(K(E)(Q(Tv)(Ht.value)("display:none;")))(P(R)(_(A)(function(s){return Q(DS)(Qd.value)(s)})(p))(_(A)(S(Q(Tv)(Ht.value)("display:block;")))(p)))))(ae)])]))}}))})}}};var RR=function(){return D.value}(),bS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sawtoothOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(M()(X)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(RR)({periodic:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.2)([iC(vE)(448)(lt())])])}}))})}}};var LR=function(){return D.value}(),yS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sine">Sine wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sine wave oscillator</a> plays back a sine wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ sinOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(M()(X)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(LR)({periodic:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.2)([Rf(Qi)(448)(lt())])])}}))})}}};var qR=function(){return D.value}(),AS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Square wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ squareOsc 448.0 bangOn ] ]</code></pre>

  ~periodic~
  </section>
`}})()()(M()(X)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(qR)({periodic:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.2)([v_(bc)(448)(lt())])])}}))})}}};var BR=function(){return D.value}(),kS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/StereoPannerNode">stereo panner</a> pans audio in the stereo plane. <code>-1.0</code> represents hard left, and <code>1.0</code> represents hard right, as in the example below.</p>

  <pre><code>\\buf -> run2_
  [ pan_ 1.0 [ loopBuf buf bangOn ] ]</code></pre>

  ~pan~
  </section>
`}})()()(M()(X)({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))(BR)({pan:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){return st(n)([rC(mE)(W(a)(a)(a))(1)([ue(Ce)(u)(lt())])])}}))})}}};var HR=function(){return D.value}(),gS=$t({reflectType:function(){return`<ul>
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
`}})()()(X)(HR)({});var VR=function(){return D.value}(),ES=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="sawtooth">Sawtooth wave oscillator</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode">sawtooth wave oscillator</a> plays back a sawtooth wave at a given frequency.</p>


  <pre><code>\\buf -> run2_
  [ gain_ 0.2 [ triangleOsc 448.0 bangOn ] ]
</code></pre>

  ~periodic~
  </section>
`}})()()(M()(X)({reflectType:function(){return"periodic"}})({reflectSymbol:function(){return"periodic"}}))(VR)({periodic:O(et(a)(a))(xt(r)(t)(function(n){return L(va)(void 0)})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(.2)([fs(Zp)(448)(lt())])])}}))})}}};var jR=function(){return D.value}(),CS=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2 id="waveshaper">Waveshaper</h2>
  <p>The <a href="https://developer.mozilla.org/en-US/docs/Web/API/WaveshaperNode">waveshaper node</a>, aka distortion, uses a <a href="https://en.wikipedia.org/wiki/Waveshaper">waveshaping function</a> to add warmth to a sound.</p>

  <pre><code>~code~</code></pre>

  ~waveShaper~
  </section>
`}})()()(M()(M()(X)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(jR)({code:O(N(a)(a))(cr(`do
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
    [ waveShaper wicked [ loopBuf buf bangOn ] ]`)),waveShaper:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/339/339822_5121236-lq.mp3")})(function(n){return function(u){var o=function(c){var i=Hi/180;return _(Pe)(function(l){var p=Xe(l)*2/Xe(44100)-1;return(3+c)*p*20*i/(Hi+c*jb(Ra)(ff)(p))})(sn(0)(44099))};return function(){var i=Id(o(400))();return st(n)([QE(zE)(W(a)(a)(a))(i)([ue(Ce)(u)(lt())])])()}}}))})}}};var QR=function(){return D.value}(),SS=function(t){return function(e){return function(r){return function(n){var u=H(J)(e(Lf.value))(_n),o=Yn(t)(r);return $t({reflectType:function(){return`<div>
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
</div>`}})()()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(on()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(M()(X)({reflectType:function(){return"waveShaper"}})({reflectSymbol:function(){return"waveShaper"}}))({reflectType:function(){return"triangleOsc"}})({reflectSymbol:function(){return"triangleOsc"}}))({reflectType:function(){return"toc"}})({reflectSymbol:function(){return"toc"}}))({reflectType:function(){return"squareOsc"}})({reflectSymbol:function(){return"squareOsc"}}))({reflectType:function(){return"sinOsc"}})({reflectSymbol:function(){return"sinOsc"}}))({reflectType:function(){return"sawtoothOsc"}})({reflectSymbol:function(){return"sawtoothOsc"}}))({reflectType:function(){return"recorder"}})({reflectSymbol:function(){return"recorder"}}))({reflectType:function(){return"playBuf"}})({reflectSymbol:function(){return"playBuf"}}))({reflectType:function(){return"periodicOsc"}})({reflectSymbol:function(){return"periodicOsc"}}))({reflectType:function(){return"peaking"}})({reflectSymbol:function(){return"peaking"}}))({reflectType:function(){return"pan"}})({reflectSymbol:function(){return"pan"}}))({reflectType:function(){return"notch"}})({reflectSymbol:function(){return"notch"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"microphone"}})({reflectSymbol:function(){return"microphone"}}))({reflectType:function(){return"lowshelf"}})({reflectSymbol:function(){return"lowshelf"}}))({reflectType:function(){return"lowpass"}})({reflectSymbol:function(){return"lowpass"}}))({reflectType:function(){return"loopBuf"}})({reflectSymbol:function(){return"loopBuf"}}))({reflectType:function(){return"iirFilter"}})({reflectSymbol:function(){return"iirFilter"}}))({reflectType:function(){return"highshelf"}})({reflectSymbol:function(){return"highshelf"}}))({reflectType:function(){return"highpass"}})({reflectSymbol:function(){return"highpass"}}))({reflectType:function(){return"gain"}})({reflectSymbol:function(){return"gain"}}))({reflectType:function(){return"drumroll"}})({reflectSymbol:function(){return"drumroll"}}))({reflectType:function(){return"delay"}})({reflectSymbol:function(){return"delay"}}))({reflectType:function(){return"convolution"}})({reflectSymbol:function(){return"convolution"}}))({reflectType:function(){return"constant"}})({reflectSymbol:function(){return"constant"}}))({reflectType:function(){return"compression"}})({reflectSymbol:function(){return"compression"}}))({reflectType:function(){return"bandpass"}})({reflectSymbol:function(){return"bandpass"}}))({reflectType:function(){return"analyser"}})({reflectSymbol:function(){return"analyser"}}))({reflectType:function(){return"allpass"}})({reflectSymbol:function(){return"allpass"}}))(QR)({drumroll:O(et(a)(a))(Ds("\u{1F941}")(n)(o)(function(c){return Et(c)("https://freesound.org/data/previews/50/50711_179538-lq.mp3")})(function(c){return function(i){return st(c)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(i)(lt())])])}})),toc:O(N(a)(a))(gS),allpass:O(N(a)(a))(NC(o)(e)(n)),analyser:O(N(a)(a))(jC(o)(e)(n)),bandpass:O(N(a)(a))(XC(o)(e)(n)),constant:O(N(a)(a))(tS(o)(e)(n)),compression:O(N(a)(a))(QC(o)(e)(n)),convolution:O(N(a)(a))(eS(o)(e)(n)),delay:O(N(a)(a))(rS(o)(e)(n)),gain:O(N(a)(a))(nS(o)(e)(n)),highpass:O(N(a)(a))(aS(o)(e)(n)),highshelf:O(N(a)(a))(uS(o)(e)(n)),iirFilter:O(N(a)(a))(oS(o)(e)(n)),loopBuf:O(N(a)(a))(iS(o)(e)(n)),lowshelf:O(N(a)(a))(cS(o)(e)(n)),lowpass:O(N(a)(a))(fS(o)(e)(n)),notch:O(N(a)(a))(_S(o)(e)(n)),playBuf:O(N(a)(a))(mS(o)(e)(n)),peaking:O(N(a)(a))(pS(o)(e)(n)),microphone:O(N(a)(a))(lS(o)(e)(n)),pan:O(N(a)(a))(kS(o)(e)(n)),periodicOsc:O(N(a)(a))(sS(o)(e)(n)),recorder:O(N(a)(a))(dS(o)(e)(n)),sawtoothOsc:O(N(a)(a))(bS(o)(e)(n)),sinOsc:O(N(a)(a))(yS(o)(e)(n)),squareOsc:O(N(a)(a))(AS(o)(e)(n)),triangleOsc:O(N(a)(a))(ES(o)(e)(n)),waveShaper:O(N(a)(a))(CS(o)(e)(n)),next:Oa(R)(E)(n)(u)})}}}};var Yd=function(){function t(){}return t.value=new t,t}(),hS={attr:function(t){return function(e){return d({key:"checked",value:q(e)})}}};var bo=function(){function t(){}return t.value=new t,t}();var Go={attr:function(t){return function(e){return d({key:"type",value:q(e)})}}};var yo=function(t){return function(e){return function(r){return V("input")(e)(F(t)(r))}}};var tN=function(t){return t},Ss=function(t){return function(e){return function(r){return _o(t)(P(t.Plus0().Alt0())(K(t)(e))(r))}}};var $_=function(t){return function(e){return t(e)}},Zi=function(t){return{map:function(e){return function(r){return function(n){return r(_(t)(function(u){return function(o){return u(e(o))}})(n))}}}}},ki=function(t){return function(e){return function(r){return function(n){return $_(_(Zi(t.Filterable1().Functor1()))(e)(r))(_(t.Filterable1().Functor1())(zf)(n))}}}};var Bc=function(t){return ki(t)(S)};var au=tN;var TS=function(t){return function(e){return au(function(r){return Dr(E)(P(R)(K(E)($_(t)(r)))(_(A)(function(n){return $_(n)(r)})(e)))})}},Zd=function(t){return{apply:function(e){return function(r){return function(n){return r(e(_(t)(Ao(zo))(n)))}}},Functor0:function(){return Zi(t)}}};var hs="_____$__$_$$_vbus";function tb(t){return t[hs]=hs,t}function eb(t){return()=>{for(let e in t)delete t[e]}}function rb(t){return()=>{let e=(o,c,i,l)=>{let p=Object.keys(l);for(var s=0;s<p.length;s++)if(l[p[s]]instanceof Object&&l[p[s]][hs]===hs){let m={},v={};e(o,m,v,l[p[s]]),c[p[s]]=m,i[p[s]]=v}else{let m=`${Math.random()}`;o[m]={},c[p[s]]=v=>()=>{let f=Object.keys(o[m]);for(var b=0;b<f.length;b++)o[m][f[b]](v)()},i[p[s]]=v=>()=>{let f=`${Math.random()}`;return o[m][f]=v,()=>{delete o[m][f]}}}},r={},n={},u={};return e(r,n,u,t),{p:n,e:u,s:r}}}var Bn={vb:function(t){return function(e){return function(r){return{}}}}},Ts=function(t){return t.vb},Nu=function(){return function(t){return function(e){return function(r){var n=Ts(t)(D.value)(D.value)(D.value);return It(function(u){return function(){var c=rb(n)();return u(r(c.p)(c.e))(),eb(c.s)}})}}}},Lu=function(t){return function(){return function(){return function(){return function(e){return function(r){return function(){return function(){return function(){return function(){return{vb:function(n){return function(u){return function(o){return Gl(t)()()(D.value)(tb(Ts(e)(D.value)(D.value)(D.value)))(Ts(r)(D.value)(D.value)(D.value))}}}}}}}}}}}}}},_r=function(t){return function(){return function(){return function(e){return function(){return function(){return function(){return function(){return{vb:function(r){return function(n){return function(u){return Gl(t)()()(D.value)(void 0)(Ts(e)(D.value)(D.value)(D.value))}}}}}}}}}}}};function xS(t){return function(e){return function(){return setTimeout(e,t)}}}function FS(t){return function(){clearTimeout(t)}}var xs=xS;var nN={eq:function(t){return function(e){return t===e}}},Fs={compare:function(t){return function(e){return Kt(Zr)(t)(e)}},Eq0:function(){return nN}};var w_=FS;var Hc=function(t){return function(e){return It(function(r){return gt(e)(function(n){return function(){var o=h_(t)();return r({acTime:o,value:n})()}})})}};var wS=function(t){return function(e){return function(r){var n=function(u){return function(o){return function(c){return function(i){return function(l){return function(p){return function(){var m=Me(c)();return En(tt)(m)(function(){var f=h_(t)(),b=xs(WA(Su(Ra)(o-f-.04)(.01)*1e3))(function(){var vt=Me(c)();return En(tt)(vt)(function(){return je(o)(l)(),u(o)(),n(u)(o+p)(c)(i)(l)(p)()})()})();return je(new B(b))(i)()})()}}}}}}};return It(function(u){return function(){var c=ge(!0)(),i=ge(z.value)(),l=h_(t)(),p=ge(l+e)();n(u)(e)(c)(i)(p)(e)();var s=gt(r)(function(m){return function(){nt(Ge)(Me(i))(mr(tt)(ur)(w_))();var f=Me(p)();return n(u)(f+m)(c)(i)(p)(m)()}})();return H(J)(H(J)(s)(je(!1)(c)))(nt(Ge)(Me(i))(mr(tt)(ur)(w_)))}})}}};var Ia=function(t){return function(e){return function(r){return function(n){return function(u){var o=r===t||n===e;if(o)return e;var c=(n-e)/(r-t),i=e-c*t;return c*u+i}}}}};var aN=function(){return D.value}(),PS=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
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

</section>`}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(aN)({txt:O(N(a)(a))(cr(`module Main where

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
  )`)),empl:O(et(a)(a))(Nu()(Lu({reflectSymbol:function(){return"cbx"}})()()()(_r({reflectSymbol:function(){return"cbx0"}})()()(_r({reflectSymbol:function(){return"cbx1"}})()()(_r({reflectSymbol:function(){return"cbx2"}})()()(_r({reflectSymbol:function(){return"cbx3"}})()()(Bn)()()()())()()()())()()()())()()()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())(Bn)()()()())()()()())(D.value)(function(u){return function(o){var c=P(R)(K(E)(void 0))(o.startStop.start),i=function(v){return Ss(E)(!1)(Pu(E)(S(lu(Wa)))(v)(!1))},l=i(o.cbx.cbx3),p=i(o.cbx.cbx2),s=i(o.cbx.cbx1),m=i(o.cbx.cbx0);return F(N(a)(a))(Ue($e(a)(a))([An($e(a)(a))(Lr(wt)(g)(_(A)(function(){var v=Q(dr)(sr.value);return function(f){return v(rr(S(f)))}}()))([ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(v){return v.value0})(n)))(j(A)(c)(Y(Z))))(function(v){return function(){v();var b=Kn(fr)(),ct=Ru(fr)(b)(),vt=function(xe){return function(Hr){return function(it){return Yl(E)(function(oe){return function(be){var qt=be.value1+(oe.value1-be.value0)*function(){return oe.value0?xe:1}();return new rt(new rt(oe.value1,qt),qt)}})(ki(E)(rt.create)(Hr)(it))(new rt(0,0))}}},Qt=Wc(b)(ji(_(A)(function(){var xe=Ve(Aa)(.04);return function(Hr){return xe(function(it){return it.acTime}(Hr))}}())(Hc(b)(Yi)))(function(xe){var Hr=function(kr){return function(Rr){return _o(E)(xe)(_(A)(tl)(_o(E)(Rr)(_(A)(function(da){return function(Un){return function(ba){return{f:da,a:Un,t:ba}}}})(kr))))}},it=_(A)(function(kr){return kr?4:1})(Bc(E)(l)(xe)),oe=vt(4)(p)(xe),be=_(A)(function(kr){return kr?4:1})(Bc(E)(s)(xe)),qt=vt(8)(m)(xe);return[Ur(ot)(W(a)(a)(a))(0)(ar(A)(Hr(qt)(be))(function(kr){return gn()(Wr)({n:Ia(1)(.01)(4)(.15)(kr.a)*sp(Hi*kr.f)+.15,o:kr.t,t:Ro})}))([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:325.6,spec:new rt(ke(de(Qn)()(Gr)()(Ua))(.3)(ke(de(xa)()(Mr)()(Gr))(-.1)(ke(de(Fa)()(xr)()(Mr))(.7)(ke(de(ia)()(sa)()(xr))(-.4)($a)))),ke(de(Qn)()(Gr)()(Ua))(.6)(ke(de(xa)()(Mr)()(Gr))(.3)(ke(de(Fa)()(xr)()(Mr))(.2)(ke(de(ia)()(sa)()(xr))(0)($a)))))})(tr(wt)(g)([lt(),ar(A)(Hr(oe)(it))(function(kr){return Do()(Wr)({n:325.6+Ia(1)(3)(4)(15.5)(kr.a)*sp(Hi*kr.f),o:kr.t,t:Ro})})]))])]}))(),ee=H(J)(H(J)(Qt)(ct))(kn(fr)(b));return t(H(J)(ee)(u.startStop.start(void 0)))(),u.startStop.stop(ee)()}}),ar(A)(o.startStop.stop)(function(v){return H(J)(v)(H(J)(t(L(tt)(void 0)))(u.startStop.start(void 0)))})]))([un(tr(wt)(g)([j(A)(c)("Turn on"),j(A)(o.startStop.stop)("Turn off")]))]),Fe($e(a)(a))(Lr(wt)(g)(_(A)(Q(pt)(Ht.value)))([j(A)(o.startStop.stop)("display:block;"),j(A)(c)("display:none;")]))(_(Pe)(function(v){return yo(N(a)(a))(tr(wt)(g)([K(E)(Q(Go)(bo.value)("checkbox")),K(E)(Q(dr)(sr.value)(rr(S(v(void 0))))),j(A)(c)(Q(hS)(Yd.value)("false"))]))(ae)})(Bs(Pe)([function(v){return v.cbx0},function(v){return v.cbx1},function(v){return v.cbx2},function(v){return v.cbx3}])(u.cbx)))]))}}))})}}}};var nb={recip:function(t){return 1/t},Ring0:function(){return ff}};var ab=function(t){return function(e){return{EuclideanRing0:function(){return t},DivisionRing1:function(){return e}}}};function zc(t){return function(){return function(e){return t(e)()}}}function Vc(t){return function(e){return function(r){return function(n){return function(){return n.addEventListener(t,e,r)}}}}}function Jc(t){return function(e){return function(r){return function(n){return function(){return n.removeEventListener(t,e,r)}}}}}function ub(t){return t.clientX}function ob(t){return t.clientY}function P_(t){return t.button}var M_=Nt("MouseEvent");var MS=function(t){return function(e){return It(function(r){return gt(e)(function(n){return function(){var o=Me(t.buttons)();return r({value:n,buttons:o})()}})})}};var OS=function(){var e=ge(z.value)(),r=ge(Qm)(),n=_(C)(jd)(Ai)(),u=zc(function(l){return mr(tt)(ur)(function(p){return je(new B({x:ub(p),y:ob(p)}))(e)})(M_(l))})(),o=zc(function(l){return mr(tt)(ur)(function(p){return Oi(UA(Zr)(P_(p)))(r)})(M_(l))})(),c=zc(function(l){return mr(tt)(ur)(function(p){return Oi(vp(Zr)(P_(p)))(r)})(M_(l))})();Vc(en()("mousemove"))(u)(!1)(n)(),Vc(en()("mousedown"))(o)(!1)(n)(),Vc(en()("mouseup"))(c)(!1)(n)();var i=function(){return Jc(en()("mousemove"))(u)(!1)(n)(),Jc(en()("mousedown"))(o)(!1)(n)(),Jc(en()("mouseup"))(c)(!1)(n)()};return{position:e,buttons:r,dispose:i}},IS=It(function(t){return function(){var r=_(C)(jd)(Ai)(),n=zc(function(u){return mr(tt)(ur)(function(o){return t(P_(o))})(M_(u))})();return Vc(en()("mousedown"))(n)(!1)(r)(),Jc(en()("mousedown"))(n)(!1)(r)}});var NS=function(t){return au(function(e){return _(A)(function(r){return r.value(r.buttons)})(MS(t)(e))})};var cb=function(t){return t};function Ps(){return Date.now()}var fh=function(t){return It(function(e){return gt(t)(function(r){return function(){var u=Ps();return e({time:u,value:r})()}})})};var Wu=function(t){return function(e){return It(function(r){return function(){var u=ge(De(zA(Fs)))(),o=gt(e)(function(c){return function(){var l=ge(z.value)(),p=xs(t)(function(){r(c)();var m=Me(l)();return jr(L(tt)(void 0))(function(v){return na(vp(Fs)(v))(u)})(m)()})();return je(new B(p))(l)(),na(St(Xm(Fs))(GA(p)))(u)()}})();return function(){var i=Me(u)();return aa(tt)(HA)(i)(w_)(),o()}}})}};var WN=au(function(t){return _(A)(function(e){return e.value(e.time)})(fh(t))}),_b=_(Zi(A))(function(){var t=b_(kC);return function(e){return t(cb(e))}}())(WN);var GN=function(t){var e=function(o){return function(c){return function(i){return function(l){return function(p){return function(s){return function(m){var v=Ve(c.DivisionRing1().Ring0().Semiring0())(ka(c.DivisionRing1().Ring0().Semiring0()))(ka(c.DivisionRing1().Ring0().Semiring0())),f=function(b){return function(ct){if(b.last instanceof z)return ct;if(b.last instanceof B)return Ve(i)(ct)(l(function(vt){return Vu(c.EuclideanRing0())(wn(c.DivisionRing1().Ring0().Semiring0())(vt(Ve(i)(b.last.value0.value1)(b.now.value1)))(Eu(c.DivisionRing1().Ring0())(b.now.value0)(b.last.value0.value0)))(v)}));throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 102, column 5 - line 102, column 35): "+[b.constructor.name,ct.constructor.name])}};return au(function(b){var ct=$_(m)(j(o.Filterable1().Functor1())(b)(Y(Z))),vt=Bp(o)(ki(o)(rt.create)(s)(ct)),Qt=Pu(o)(f)(vt)(p);return _o(o)(Qt)(b)})}}}}}}},r=function(o){return function(c){return e(o)(c)(c.DivisionRing1().Ring0().Semiring0())(function(i){return i(Y(Z))})}},n=function(o){return function(c){return au(function(i){return Zl(E)(function(l){var p=c(Ss(E)(o)(l));return{input:Bc(E)(p)(i),output:_o(E)(l)(i)}})})}},u=function(o){return function(c){return function(i){if(BA(o))return-8*(c-1)-i*2;if(nr)return 2*(4-c);throw new Error("Failed pattern match at WAGS.Example.Docs.FixEx (line 62, column 3 - line 64, column 34): "+[o.constructor.name,c.constructor.name,i.constructor.name])}}};return n(2)(function(o){return r(E)(ab(ll)(nb))(2)(_(Zi(A))(In())(_b))(function(){var c=n(10)(function(i){return r(E)(ab(ll)(nb))(10)(_(Zi(A))(In())(_b))(Vt(Zd(A))(Vt(Zd(A))(_(Zi(A))(u)(NS(t)))(o))(i))});return TS(c)(j(A)(IS)(c))}())})},BN=function(){return D.value}(),ch=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<section>
  <h2>Fix</h2>

  <p>Fix, like it's equivalent in wags that we've already seen, creates a feedback loop. However, in this case, we are talking about a feedback loop of <i>events</i>, not sound.</p>

  <p>At first glance, it may not be clear why we need an event stream to feed back into itself? It seems prone to saturation: if you have a counter that feeds back into itself with a delay, after a few seconds you'll have so many events that it will crash your browser (I've tried it!).</p>

  <p>However, there's one important circumstance where you need fixed points: when an event can only be defined in terms of itself. One classic category of this is the <i>differential equation</i>. Differential equations allow you to produce <a href="https://en.wikipedia.org/wiki/Simple_harmonic_motion">Slinky effects, aka simple harmonic motion,</a> and a lot of other neat behaviors that are difficult to produce via other means.</p>

  <p>Let's listen to the sound of simple harmonic motion in the example below, courtesy of <code>fix</code>. The differential equation in the example below comes from Phil Freeman, the creator of the PureScript language and the author of the <code>purescript-behaviors</code> package. When you click "Turn on", you won't hear much, but press and release your mouse anywhere on the screen to hear the differential equation take flight!</p>

  <pre><code>~txt~</code></pre>

  ~empl~

  <p>When working with stateful events, a good way to decide if you should use <code>fold</code> versus <code>fix</code> is to ask the following question: can I incrementally change my state based on an initial state, or is my state defined in terms of how it changes? If you can incrementally change your state, go with <code>fold</code>. If, on the other hand, your state is defined in terms of how it changes, go with <code>fix</code>.</p>
</section>`}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"empl"}})({reflectSymbol:function(){return"empl"}}))(BN)({txt:O(N(a)(a))(cr(`module Main where

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
  )`)),empl:O(et(a)(a))(Nu()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())(D.value)(function(u){return function(o){var c=P(R)(K(E)(void 0))(o.start);return F(N(a)(a))(Ue($e(a)(a))([An($e(a)(a))(Lr(wt)(g)(_(A)(function(){var i=Q(dr)(sr.value);return function(l){return i(rr(S(l)))}}()))([ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(i){return i.value0})(n)))(j(A)(c)(Y(Z))))(function(i){return function(){i();var p=Kn(fr)(),s=Ru(fr)(p)(),m=OS(),v=Xl(0)(1e4)(),f=function(it){return{o:it.value0+.04,n:it.value1,t:Ro}},b=_(lo)(function(it){return it-.5})(dv(gk)),ct=nt(Cf)(b)(function(it){return nt(Cf)(b)(function(oe){return nt(Cf)(b)(function(be){return nt(Cf)(b)(function(qt){return L(Ql)(ke(de(Qn)()(Gr)()(Ua))(it)(ke(de(xa)()(Mr)()(Gr))(oe)(ke(de(Fa)()(xr)()(Mr))(be)(ke(de(ia)()(sa)()(xr))(qt)($a)))))})})})}),vt=Vt(Sf)(_(lo)(rt.create)(ct))(ct),Qt=Vt(Sf)(Vt(Sf)(Vt(Sf)(_(lo)(function(it){return function(oe){return function(be){return function(qt){return{s0:it,s1:oe,s2:be,s3:qt}}}}})(vt))(vt))(vt))(vt),ee=Pp(Qt)({newSeed:wp(v),size:5}),xe=Wc(p)(ji(_(A)(function(it){return new rt(it.acTime,it.value)})(Hc(p)(Bc(E)(GN(m))(Yi))))(function(it){return[Ur(ot)(W(a)(a)(a))(0)(_(A)(function(){var oe=gn()(Wr),be=qn(vn)(function(qt){return Su(Ra)(-.4)(.5*(qt-1))});return function(qt){return oe(f(be(qt)))}}())(it))([wc(bD(kt(At()(U(U(yt)(EE)()()()({reflectSymbol:function(){return"q"}}))(oD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:90.4,q:20})([uC(bc)(90.4)])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(function(){var oe=gn()(Wr),be=qn(vn)(function(qt){return Su(Ra)(-.2)(.4*(qt-3))});return function(qt){return oe(f(be(qt)))}}())(it))([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:90.4*4,q:20})([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*3.02,spec:ee.s0})(P(R)(lt())(_(A)(function(){var oe=Do()(Wr),be=qn(vn)(function(qt){return 90.4*3.02+14*(qt-1)});return function(qt){return oe(f(be(qt)))}}())(it)))])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(function(){var oe=gn()(Wr),be=qn(vn)(function(qt){return Su(Ra)(-.1)(.2*(qt-6))});return function(qt){return oe(f(be(qt)))}}())(it))([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:90.4*6,q:20})([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*5.07,spec:ee.s1})(P(R)(lt())(_(A)(function(){var oe=Do()(Wr),be=qn(vn)(function(qt){return 90.4*5.07+18*(qt-1)});return function(qt){return oe(f(be(qt)))}}())(it)))])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(function(){var oe=gn()(Wr),be=qn(vn)(function(qt){return Su(Ra)(0)(.2*(qt-3))});return function(qt){return oe(f(be(qt)))}}())(it))([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:90.4*8,q:20})([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*7.13,spec:ee.s2})(P(R)(lt())(_(A)(function(){var oe=Do()(Wr),be=qn(vn)(function(qt){return 90.4*7.13+32*(qt-1)});return function(qt){return oe(f(be(qt)))}}())(it)))])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(function(){var oe=gn()(Wr),be=qn(vn)(function(qt){return Su(Ra)(0)(.1*(qt-7))});return function(qt){return oe(f(be(qt)))}}())(it))([vi(mi(kt(At()(U(U(yt)(si(_i(Qn)))()()()({reflectSymbol:function(){return"spec"}}))(pi)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))({frequency:90.4*9.14,spec:ee.s3})(P(R)(lt())(_(A)(function(){var oe=Do()(Wr),be=qn(vn)(function(qt){return 90.4*9.14+31*(qt-1)});return function(qt){return oe(f(be(qt)))}}())(it)))])]}))(),Hr=H(J)(H(J)(xe)(s))(kn(fr)(p));return t(H(J)(Hr)(u.start(void 0)))(),u.stop(Hr)()}}),ar(A)(o.stop)(function(i){return H(J)(i)(H(J)(t(L(tt)(void 0)))(u.start(void 0)))})]))([un(tr(wt)(g)([j(A)(c)("Turn on"),j(A)(o.stop)("Turn off")]))])]))}}))})}}}};var HN=function(){return D.value}(),lh=function(t){return function(e){return function(r){return function(n){var u=Yn(t)(r);return $t({reflectType:function(){return`<div>
  <h1>State</h1>

  <h3>Or Events 2.0</h3>
  <p>
    The name of this section is a bit of a nisnomer. While it will address the issue of maintaining state in an audio graph, it's really just about two mechanisms you can use to make an <code>Event</code> stateful. One is called <code>fold</code>, and the other is called <code>fix</code>. Both are part of the <code>IsEvent</code> typeclass, which means you get them for free when working with events.
  </p>

  ~fold~
  ~fix~

  <h2>Next steps</h2>
  <p>Using <code>fold</code> and <code>fix</code>, we can create internal state in our Web Audio works that would be really tedious and error-prone to achieve in vanilla JS or other compile-to-JS languages. There's still one nagging issue that we haven't addressed, though. For all of the flexibility we can achieve with events, we still can't flex the audio graph itself, meaning that we can't add or remove components. In the next section, we'll learn how to do that with <a ~next~ style="cursor:pointer;">subgraphs</a>.</p>
</div>`}})()()(M()(M()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"fold"}})({reflectSymbol:function(){return"fold"}}))({reflectType:function(){return"fix"}})({reflectSymbol:function(){return"fix"}}))(HN)({next:Oa(R)(E)(n)(H(J)(e(C_.value))(_n)),fold:O(N(a)(a))(PS(u)(e)(r)(n)),fix:O(N(a)(a))(ch(u)(e)(r)(n))})}}}};var VN=function(){function t(){}return t.value=new t,t}(),_h=function(){function t(){}return t.value=new t,t}(),pb=function(){function t(e){this.value0=e}return t.create=function(e){return new t(e)},t}(),JN=`module Main where

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
`;var jN=function(){return D.value}(),XN=function(t){return function(e){return function(r){return K(t)(Gf(e)(wf)({x:aD,o:r}))}}},QN=function(t){return function(e){return function(r){return K(t)(Gf(e)(wf)({x:_E,o:r}))}}},KN=po(za)(Xe)(function(t){var e=function(u){return P(R)(XN(E)()(u+.27*(t*Ui(1.005)(t))))(QN(E)()(u+3+.3*(t*Ui(1.005)(t))))},r=function(u){return K(E)(gn()(xn)({p:[0,.4,.1,.05,.01,0],o:u+.3*(t*Ui(1.005)(t)),d:.8}))},n=function(u){return function(o){return Ur(ot)(W(a)(a)(a))(0)(r(u))([Rf(Qi)(200+t*o)(e(u))])}};return[n(.2)(4),n(.3)(6),n(.45)(14),n(.7)(20)]}),ph=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(D.value)(jN)({txt:O(N(a)(a))(cr(JN)),ex0:O(et(a)(a))(an(function(n){return po(za)(function(u){return P(R)(K(E)(VN.value))(u)})(function(u){return F(N(a)(a))(Ue($e(a)(a))([An($e(a)(a))(ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(o){return o.value0})(r)))(_(A)(rt.create)(u)))(function(o){return Q(dr)(sr.value)(rr(S(function(){return o.value0 instanceof pb?H(J)(H(J)(o.value0.value0)(n(_h.value)))(t(L(tt)(void 0))):function(){o.value1();var i=ds([Rt(ot)(W(a)(a)(a))(1)(ra(Vo)(_(Pe)(KN)(sn(0)(100))))])();return t(H(J)(i)(n(_h.value)))(),n(new pb(i))()}}())))}))([un(ar(A)(u)(function(o){return o instanceof pb?"Turn off":"Turn on"}))])]))})}))})}}};var gi=function(){function t(){}return t.value=new t,t}();var ef={attr:function(t){return function(e){return d({key:"max",value:q(e)})}}};var Ei=function(){function t(){}return t.value=new t,t}();var rf={attr:function(t){return function(e){return d({key:"min",value:q(e)})}}};var Ci=function(){function t(){}return t.value=new t,t}();var nf={attr:function(t){return function(e){return d({key:"input",value:ft(e)})}}};var Si=function(){function t(){}return t.value=new t,t}(),af={attr:function(t){return function(e){return d({key:"step",value:q(e)})}}};var hi=function(){function t(){}return t.value=new t,t}();var uf={attr:function(t){return function(e){return d({key:"value",value:q(e)})}}};function sh(t){return t.target}var Xc=function(t){return Xr(sh(t))};var e1=`module Main where

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
import WAGS.Core (bangOn)
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
`,r1=function(){return D.value}(),n1="https://freesound.org/data/previews/100/100981_1234256-lq.mp3",mh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(M()(M()(X)({reflectType:function(){return"wagtxt"}})({reflectSymbol:function(){return"wagtxt"}}))({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(r1)({wagtxt:O(N(a)(a))(cr(`run2_
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
      ]`)),txt:O(N(a)(a))(cr(e1)),ex1:O(et(a)(a))(Nu()(Lu({reflectSymbol:function(){return"slider"}})()()()(_r({reflectSymbol:function(){return"s0"}})()()(_r({reflectSymbol:function(){return"s1"}})()()(_r({reflectSymbol:function(){return"s2"}})()()(Bn)()()()())()()()())()()()())(Lu({reflectSymbol:function(){return"startStop"}})()()()(_r({reflectSymbol:function(){return"loading"}})()()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())()()()())(Bn)()()()())()()()())(D.value)(function(n){return function(u){var o=P(R)(u.startStop.start)(K(E)(void 0)),c=function(i){return ue(Of(kt(At()(U(U(U(U(yt)(Cc)()()()({reflectSymbol:function(){return"playbackRate"}}))(__)()()()({reflectSymbol:function(){return"loopStart"}}))(l_)()()()({reflectSymbol:function(){return"loopEnd"}}))(Mf)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:i,playbackRate:2.6,loopStart:.6,loopEnd:1.1})(tr(wt)(g)([lt(),_(A)(function(){var l=Zn()(Yp),p=Ia(0)(.2)(100)(5);return function(s){return l(p(s))}}())(u.slider.s0),_(A)(function(){var l=YC(),p=Ia(0)(0)(100)(1.2);return function(s){return l(p(s))}}())(u.slider.s1),_(A)(function(){var l=ZC(),p=Ia(0)(.05)(100)(1);return function(s){return l(p(s))}}())(Tn(E)(u.slider.s2)(_(A)(Ve(Aa))(P(R)(K(E)(0))(u.slider.s1))))]))};return F(N(a)(a))(Ue($e(a)(a))(St(uu)(_(Pe)(function(i){return Ue($e(a)(a))([cr(i.l),yo(N(a)(a))(Lr(wt)(g)(K(E))([Q(Go)(bo.value)("range"),Q(rf)(Ei.value)("0"),Q(ef)(gi.value)("100"),Q(af)(Si.value)("1"),Q(uf)(hi.value)("50"),Q(nf)(Ci.value)(rr(function(){var l=mr(tt)(ur)(Jf(Ge)(qf)(i.f)),p=zn(Ea)(qc);return function(s){return l(p(Xc(s)))}}()))]))(ae)])})([{l:"Playback rate",f:n.slider.s0},{l:"Loop start",f:n.slider.s1},{l:"Loop end",f:n.slider.s2}]))([An($e(a)(a))(Lr(wt)(g)(_(A)(function(){var i=Q(dr)(sr.value);return function(l){return i(rr(S(l)))}}()))([j(A)(u.startStop.loading)(L(tt)(void 0)),ar(A)(u.startStop.stop)(function(i){return H(J)(i)(H(J)(t(L(tt)(void 0)))(n.startStop.start(void 0)))}),ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(i){return i.value0})(r)))(j(A)(o)(Y(Z))))(function(i){return function(){i(),n.startStop.loading(void 0)();var p=vo(nt(Ir)(Kn(Or))(function(s){return nt(Ir)(Ru(Or)(s))(function(m){return nt(Ir)(Et(s)(n1))(function(v){return vr(Or)(function(){var b=st(s)([c(v)])(),ct=H(J)(H(J)(b)(m))(kn(fr)(s));return n.startStop.stop(ct)(),ct})})})}))();return t(function(){return n.startStop.start(void 0)(),Lo(bi(p))()})(),void 0}})]))([un(tr(wt)(g)([_(A)(S("Turn off"))(u.startStop.stop),_(A)(S("Turn on"))(o)]))])])))}}))})}}};var u1=`module Main where

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
import WAGS.Core (AudioEnvelope(..), bangOn)
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
  )`,o1=au(function(t){return It(function(e){return gt(t)(function(r){return function(){var u=Ka();return e(r(u))()}})})}),i1=function(){return D.value}(),f1=function(t){if(t<.142857)return 261.625565;if(t<.285714)return 293.664768;if(t<.428571)return 349.228231;if(t<.571429)return 391.995436;if(t<.714286)return 440;if(t<.857143)return 523.251131;if(nr)return 587.329536;throw new Error("Failed pattern match at WAGS.Example.Docs.Events.Ex2 (line 225, column 1 - line 225, column 23): "+[t.constructor.name])},vh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
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
`}})({reflectType:function(){return"@"}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))(D.value)(i1)({txt:O(N(a)(a))(cr(u1)),ex2:O(et(a)(a))(Nu()(_r({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())(Bn)()()()())()()()())(D.value)(function(n){return function(u){var o=P(R)(u.startStop.start)(K(E)(void 0)),c=function(i){return ji(i)(function(l){var p=_(A)(function(){var ct=Ve(Aa)(.01);return function(vt){return ct(tn(vt))}}())(l),s=_(A)(Ga)(l),m=P(R)(lt())(_(A)(function(){var ct=Do()(Yp);return function(vt){return ct(f1(vt))}}())(s)),v=_(A)(function(ct){return Qp(function(vt){return{p:[0,.15,.05,.01,.005,5e-4,0],d:.4,o:vt}}(ct))})(p),f=_(A)(function(ct){return Qp(function(vt){return{p:[0,.3,.1,.05,.01,.005,0],d:.4,o:vt}}(ct))})(p),b=_(A)(function(ct){return Qp(function(vt){return{p:[0,.6,.2,.1,.5,.03,0],d:.4,o:vt}}(ct))})(p);return[Pa(fs(Zp)(0)(m))(function(ct){return function(vt){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(2)([Ur(ot)(W(a)(a)(a))(0)(_(A)(gn()(xn))(b))([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:1e3,q:20})([ct])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(gn()(xn))(f))([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:2e3,q:20})([ct])]),Ur(ot)(W(a)(a)(a))(0)(_(A)(gn()(xn))(v))([$c(yD(kt(At()(U(U(yt)(hE)()()()({reflectSymbol:function(){return"q"}}))(iD)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:4e3,q:20})([ct])])]))}})]})};return F(N(a)(a))(Ue($e(a)(a))([Ue($e(a)(a))([cr("tempo"),yo(N(a)(a))(Lr(wt)(g)(K(E))([Q(Go)(bo.value)("range"),Q(rf)(Ei.value)("0"),Q(ef)(gi.value)("100"),Q(af)(Si.value)("1"),Q(uf)(hi.value)("50"),Q(nf)(Ci.value)(rr(function(){var i=mr(tt)(ur)(Jf(Ge)(qf)(n.slider)),l=zn(Ea)(qc);return function(p){return i(l(Xc(p)))}}()))]))(ae)]),An($e(a)(a))(Lr(wt)(g)(_(A)(function(){var i=Q(dr)(sr.value);return function(l){return i(rr(S(l)))}}()))([ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(i){return i.value0})(r)))(j(A)(o)(Y(Z))))(function(i){return function(){i();var p=Kn(fr)(),s=ki(E)(rt.create)(o1)(wS(p)(.91)(_(A)(Ia(0)(.42)(100)(1.4))(u.slider))),m=Wc(p)(c(s))(),v=H(J)(m)(kn(fr)(p));return t(H(J)(v)(n.startStop.start(void 0)))(),n.startStop.stop(H(J)(v)(kn(fr)(p)))()}}),ar(A)(u.startStop.stop)(function(i){return H(J)(i)(H(J)(t(L(tt)(void 0)))(n.startStop.start(void 0)))})]))([un(tr(wt)(g)([j(A)(o)("Turn on"),j(A)(u.startStop.stop)("Turn off")]))])]))}}))})}}};var l1=function(){return D.value}(),Dh=function(){return le({reflectType:function(){return`<section>
  <h2>Three flavors of events.</h2>

  <p>When we're in the browser, events tend to come in three broad categories:</p>

  <ul>
    <li>Things that need to happen <span style="font-weight: 800;">now</span>.</li>
    <li>Things that happen as the result of a user interaction.</li>
    <li>Things that are scheduled to happen in the future, for example with <code>setTimeout</code>.</li>
  </ul>

  <p>The next three examples cover all three cases.</p>

</section>`}})({reflectType:function(){return"@"}})()()(X)(D.value)(l1)({})}();var p1=function(){return D.value}(),dh=function(){return le({reflectType:function(){return`<section>
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
</section>`}})({reflectType:function(){return"@"}})()()(X)(D.value)(p1)({})}();var m1=function(){return D.value}(),bh=function(){return le({reflectType:function(){return`<section>

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
</section>`}})({reflectType:function(){return"@"}})()()(X)(D.value)(m1)({})}();var D1=function(){return D.value}(),yh=function(t){return function(e){return function(r){return function(n){var u=function(c){return Oa(R)(E)(n)(H(J)(e(c))(_n))},o=Yn(t)(r);return le({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(M()(M()(M()(M()(M()(on()(M()(X)({reflectType:function(){return"primer"}})({reflectSymbol:function(){return"primer"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"inWags"}})({reflectSymbol:function(){return"inWags"}}))({reflectType:function(){return"flavors"}})({reflectSymbol:function(){return"flavors"}}))({reflectType:function(){return"ex2"}})({reflectSymbol:function(){return"ex2"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))({reflectType:function(){return"ex0"}})({reflectSymbol:function(){return"ex0"}}))(D.value)(D1)({next:u(g_.value),primer:O(N(a)(a))(bh),inWags:O(N(a)(a))(dh),flavors:O(N(a)(a))(Dh),ex0:O(N(a)(a))(ph(o)(e)(n)),ex1:O(N(a)(a))(mh(o)(e)(n)),ex2:O(N(a)(a))(vh(o)(e)(n))})}}}};var b1=function(){return D.value}(),Ah=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(b1)({ai0:O(et(a)(a))(xt(r)(t)(function(n){return mo($n)(Vt(di)(Vt(di)(Vt(di)(_(Rc)(function(u){return function(o){return function(c){return function(i){return{tink0:u,tink1:o,tink2:c,tink3:i}}}}})(Fn($n)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)(function(){var o=function(c){return K(E)(Gf()(wf)(Kp()(Ve(Aa)(c))(i_)))};return[Gn(Va)(u.tink0)(o(.1)),Gn(Va)(u.tink1)(o(.2)),Gn(Va)(u.tink2)(o(.9)),Gn(Va)(u.tink3)(o(1.8))]}())])}}))})}}};var A1=function(){return D.value}(),kh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(A1)({ai0:O(et(a)(a))(xt(r)(t)(function(n){return mo($n)(Vt(di)(Vt(di)(Vt(di)(_(Rc)(function(u){return function(o){return function(c){return function(i){return{tink0:u,tink1:o,tink2:c,tink3:i}}}}})(Fn($n)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/562/562008_7107243-lq.mp3"))))(Fn($n)(Et(n)("https://freesound.org/data/previews/126/126531_2044671-lq.mp3"))))})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)(function(){var o=function(i){return K(E)(Gf()(wf)(Kp()(Ve(Aa)(i))(i_)))},c=function(i){var l=iu(Co)(i)(4);return l===0?u.tink0:l===1?u.tink1:l===2?u.tink2:u.tink3};return ar(Pe)(sn(0)(100))(function(i){var l=Xe(i);return Gn(Va)(c(i))(o(.3+.3*(l*Ui(1.005)(l))))})}())])}}))})}}};var g1=function(){return D.value}(),gh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(g1)({ai0:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Pa(ue(Ce)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(.8)([yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:400,q:1})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:880,q:5})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:1200,q:10})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:2e3,q:20})([o]),yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:3e3,q:30})([o])]))}})])}}))})}}};var C1=function(){return D.value}(),Eh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
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
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(C1)({ai0:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Pa(ue(Ce)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(Rt(ot)(W(a)(a)(a))(.8)(ar(Pe)(sn(0)(40))(po(za)(Xe)(function(i){return yn(ln(kt(At()(U(U(yt)(bn)()()()({reflectSymbol:function(){return"q"}}))(cn)()()()({reflectSymbol:function(){return"frequency"}})))(bt()())))(W(a)(a)(a))({frequency:200+i*150,q:30})([o])}))))}})])}}))})}}};var h1=function(){return D.value}(),Ch=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
  <pre><code>\\buf -> run2_
  [ fix
      \\b -> gain_ 1.0
        [ playBuf buf bangOn
        , delay_ 0.1 [ gain_ 0.6 [ b ] ]
        ]
  ]</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(h1)({ai0:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return st(n)([nu(function(o){return Rt(ot)(W(a)(a)(a))(1)([Gn(Va)(u)(lt()),so(Qr)(W(a)(a)(a))(.1)([Rt(ot)(W(a)(a)(a))(.6)([o])])])})])}}))})}}};var x1=function(){return D.value}(),F1=function(t){return function(e){return K(t)(gn(e)(xn)({p:[1,1,0],o:0,d:10}))}},$1=function(t){return function(e){return K(t)(gn(e)(xn)({p:[1,1,0],o:0,d:8}))}},Qc=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return so(t)(W(a)(a)(a))(u)([Rt(e)(W(a)(a)(a))(o)([$c(r)(n)(c)(i)])])}}}}}}}},Sh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<div>
  <pre><code>@txt@</code></pre>

  @ai0@
  </div>
`}})({reflectType:function(){return"@"}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ai0"}})({reflectSymbol:function(){return"ai0"}}))(D.value)(x1)({txt:O(N(a)(a))(cr(`dgh d g h i =
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
  ]`)),ai0:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/178/178660_717950-lq.mp3")})(function(n){return function(u){return st(n)([Pa(Gn(Va)(u)(lt()))(function(o){return function(c){return Ke(Qe(a)(a)(a))(nu(function(i){return Rt(ot)(W(a)(a)(a))(1)([o,Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.15)(.7)(1500)([nu(function(l){return Ur(ot)(W(a)(a)(a))(1)(F1(E)())([Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.4)(.5)(2500)([i,l])])})]),Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.29)(.85)(2e3)([nu(function(l){return Rt(ot)(W(a)(a)(a))(1)([Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.6)(.6)(3500)([i,nu(function(p){return Ur(ot)(W(a)(a)(a))(1)($1(E)())([Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.75)(.6)(4e3)([l,p]),Qc(Qr)(ot)(Ya)(W(a)(a)(a))(.75)(.55)(3e3)([o])])})])])})])])}))}})])}}))})}}};var P1=function(){return D.value}(),hh=function(t){return function(e){return function(r){return function(n){var u=function(c){return Oa(R)(E)(n)(H(J)(e(c))(_n))},o=Yn(t)(r);return le({reflectType:function(){return`<section>
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
  </section>`}})({reflectType:function(){return"@"}})()()(on()(X)({reflectType:function(){return"hwLink"}})({reflectSymbol:function(){return"hwLink"}}))(D.value)(P1)({hwLink:u(Nf.value)})}}}};var O1=function(){return D.value}(),Th=function(t){return function(e){return function(r){return function(n){var u=function(c){return Oa(R)(E)(n)(H(J)(e(c))(_n))},o=Yn(t)(r);return le({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(M()(M()(M()(M()(M()(M()(M()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"intro"}})({reflectSymbol:function(){return"intro"}}))({reflectType:function(){return"code5"}})({reflectSymbol:function(){return"code5"}}))({reflectType:function(){return"code4"}})({reflectSymbol:function(){return"code4"}}))({reflectType:function(){return"code3"}})({reflectSymbol:function(){return"code3"}}))({reflectType:function(){return"code2"}})({reflectSymbol:function(){return"code2"}}))({reflectType:function(){return"code1"}})({reflectSymbol:function(){return"code1"}}))({reflectType:function(){return"code0"}})({reflectSymbol:function(){return"code0"}}))(D.value)(O1)({intro:O(N(a)(a))(hh(t)(e)(r)(n)),next:u(A_.value),code0:O(N(a)(a))(Ah(o)(e)(n)),code1:O(N(a)(a))(kh(o)(e)(n)),code2:O(N(a)(a))(gh(o)(e)(n)),code3:O(N(a)(a))(Eh(o)(e)(n)),code4:O(N(a)(a))(Ch(o)(e)(n)),code5:O(N(a)(a))(Sh(o)(e)(n))})}}}};var xh=function(t){return function(e){return function(r){return V("code")(e)(F(t)(r))}}},mb=function(t){return xh(t)(T(g))};var Fh=function(t){return function(e){return function(r){return V("pre")(e)(F(t)(r))}}},vb=function(t){return Fh(t)(T(g))};var L1=function(){return D.value}(),$h=function(t){return function(e){return function(r){return function(n){var u=H(J)(e(k_.value))(_n),o=Yn(t)(r);return le({reflectType:function(){return`<div>
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
</div>`}})({reflectType:function(){return"@"}})()()(M()(on()(M()(X)({reflectType:function(){return"result"}})({reflectSymbol:function(){return"result"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"code"}})({reflectSymbol:function(){return"code"}}))(D.value)(L1)({code:O(N(a)(a))(vb($e(a)(a))([mb($e(a)(a))([cr(`case e of
  Just x -> x *> push Nothing
  _ -> (run2_ [ gain_ 0.15 [ sinOsc 440.0 bangOn ] ]
         >>= Just >>> push`)])])),result:O(et(a)(a))(xt(n)(o)(function(c){return L(va)(void 0)})(function(c){return function(i){return st(c)([Rt(ot)(W(a)(a)(a))(.15)([Rf(Qi)(440)(lt())])])}})),next:Oa(R)(E)(n)(u)})}}}};var wh=uc;var Ph=function(){return function(t){return t}};var Mh=function(){return function(t){return t}};var Db=function(){function t(){}return t.value=new t,t}();var Oh={attr:function(t){return function(e){return d({key:"height",value:q(e)})}}};var db=function(){function t(){}return t.value=new t,t}();var Ih={attr:function(t){return function(e){return d({key:"width",value:q(e)})}}};var bb=function(t){return function(e){return function(r){return V("canvas")(e)(F(t)(r))}}};var yb=function(){function t(){}return t.value=new t,t}(),Ab={attr:function(t){return function(e){return d({key:"@canvas-hack@",value:ft(e)})}}};function O_(t){return function(e){return function(){t.fillStyle=e}}}function Ls(t){return function(){t.beginPath()}}function Ws(t){return function(){t.fill()}}function kb(t){return function(e){return function(){t.arc(e.x,e.y,e.radius,e.start,e.end,e.useCounterClockwise)}}}function qs(t){return function(e){return function(){t.fillRect(e.x,e.y,e.width,e.height)}}}var rL=function(){return 2*Hi}(),Kc=function(t){return{o:t.value0+.04,n:t.value1,t:Ro}};var nL=function(){return D.value}(),Yc=function(t){return function(e){return function(r){return function(n){return K(t)(Do(e)(xn)({p:[r,n],o:0,d:16}))}}}},aL=function(t){return function(e){return K(t)(gn(e)(xn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:18}))}},uL=function(t){return function(e){return K(t)(gn(e)(xn)({p:[1,1,.75,.5,.75,.5,.75,.5,.25,.5,.25,0],o:0,d:24}))}};var Gs=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return function(l){return function(p){return function(s){return s_(t)(W(a)(a)(a))(u)(o)([Ur(e)(W(a)(a)(a))(c)(i)([TD(r)(n)(l)(p)(s)])])}}}}}}}}}}},Rh=function(t){return function(e){return function(r){return function(n){return function(u){return function(o){return function(c){return function(i){return function(l){return function(p){return function(s){return s_(t)(W(a)(a)(a))(u)(o)([Ur(e)(W(a)(a)(a))(c)(i)([xD(r)(n)(l)(p)(s)])])}}}}}}}}}}},oL=function(t){return function(e){return function(r){return function(n){return K(t)(Gc(e)(xn)({p:[r,n],o:0,d:16}))}}}},Nh=400,gb=Xe(Nh),iL=function(){return jt(Hu)(Nh)+"px"}(),Lh=600,Eb=Xe(Lh),fL=function(){return jt(Hu)(Lh)+"px"}(),cL={pluck0:"https://freesound.org/data/previews/493/493016_10350281-lq.mp3",pluck1:"https://freesound.org/data/previews/141/141524_2558140-lq.mp3",strum0:"https://freesound.org/data/previews/234/234738_3635427-lq.mp3"},Wh=function(t){return function(e){return function(r){return le({reflectType:function(){return"<section>@ex1@</section>"}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(nL)({ex1:O(et(a)(a))(Nu()(_r({reflectSymbol:function(){return"canvas"}})()()(_r({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(_r({reflectSymbol:function(){return"loading"}})()()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())()()()())(Bn)()()()())()()()())()()()())(D.value)(function(n){return function(u){var o=P(R)(K(E)(void 0))(u.startStop.start),c=function(i){return function(l){return function(p){var s=_(A)(function(m){return new rt(m.acTime,m.value)})(Hc(i)(u.slider));return[is(os(kt(At()(U(U(yt)(us)()()()({reflectSymbol:function(){return"fftSize"}}))(as)()()()({reflectSymbol:function(){return"cb"}})))(bt()())))(Qe(a)(a)(a))({cb:function(m){return function(){return je(new B(m))(p)(),je(z.value)(p)}},fftSize:jp.value})(Pa(Gn(Va)(l)(P(R)(lt())(_(A)(function(){var m=Zn()(Wr),v=qn(vn)(Ia(0)(.96)(100)(1.04));return function(f){return m(Kc(v(f)))}}())(s))))(function(m){return function(v){return Ke(Qe(a)(a)(a))(nu(function(f){return Rt(ot)(W(a)(a)(a))(1)([m,s_(AD(kt(At()(U(U(yt)(TE)()()()({reflectSymbol:function(){return"maxDelayTime"}}))(fD)()()()({reflectSymbol:function(){return"delayTime"}})))(bt()())))(W(a)(a)(a))({maxDelayTime:2.5,delayTime:1})(_(A)(function(){var b=Gc()(Wr),ct=qn(vn)(Ia(0)(.5)(100)(2.45));return function(vt){return b(Kc(ct(vt)))}}())(s))([Ur(ot)(W(a)(a)(a))(.4)(_(A)(function(){var b=gn()(Wr),ct=qn(vn)(Ia(0)(.6)(100)(.9));return function(vt){return b(Kc(ct(vt)))}}())(s))([m])]),Gs(Qr)(ot)(Ya)(W(a)(a)(a))(.15)(T(g))(.7)(T(g))(1500)(Yc(E)()(1500)(3e3))([nu(function(b){return Ur(ot)(W(a)(a)(a))(1)(aL(E)())([Gs(Qr)(ot)(Ya)(W(a)(a)(a))(.4)(T(g))(.5)(T(g))(3e3)(Yc(E)()(3e3)(100))([f,b])])})]),Gs(Qr)(ot)(Ya)(W(a)(a)(a))(.29)(_(A)(function(){var b=Gc()(Wr),ct=qn(vn)(Ia(0)(.1)(100)(.4));return function(vt){return b(Kc(ct(vt)))}}())(s))(.85)(T(g))(2e3)(Yc(E)()(2e3)(5e3))([nu(function(b){return Rt(ot)(W(a)(a)(a))(1)([Gs(Qr)(ot)(Ya)(W(a)(a)(a))(.6)(_(A)(function(){var ct=Gc()(Wr),vt=qn(vn)(Ia(0)(.8)(100)(.3));return function(Qt){return ct(Kc(vt(Qt)))}}())(s))(.6)(T(g))(3500)(Yc(E)()(3500)(100))([f,nu(function(ct){return Ur(ot)(W(a)(a)(a))(1)(uL(E)())([Rh(Qr)(ot)(kD)(W(a)(a)(a))(.75)(_(A)(function(){var vt=Gc()(Wr),Qt=qn(vn)(Ia(0)(.9)(100)(.1));return function(ee){return vt(Kc(Qt(ee)))}}())(s))(.6)(T(g))(4e3)(Yc(E)()(4e3)(200))([b,ct]),Rh(Qr)(ot)(kD)(W(a)(a)(a))(.75)(oL(E)()(.75)(.2))(.55)(T(g))(200)(Yc(E)()(200)(4e3))([m])])})])])})])])}))}}))]}}};return F(N(a)(a))(Ue($e(a)(a))([bb(N(a)(a))(P(R)(Lr(wt)(g)(K(E))([Q(Ih)(db.value)(fL),Q(Oh)(Db.value)(iL),Q(Bk)(Ht.value)("width: 100%;"),Q(Ab)(yb.value)(function(i){return function(){return O_(i)("black")(),qs(i)({width:Eb,height:gb,x:0,y:0})(),void 0}})]))(_(A)(function(i){return Q(Ab)(yb.value)(function(l){return function(){return O_(l)("black")(),qs(l)({width:Eb,height:gb,x:0,y:0})(),O_(l)("rgba(255,255,255,0.2)")(),Mi(i)(function(s){return function(){return Ls(l)(),kb(l)({end:rL,radius:s.value1*40,start:0,x:s.value0.x*Eb,y:s.value0.y*gb,useCounterClockwise:!1})(),Ws(l)()}})()}})})(u.canvas)))(ae),yo(N(a)(a))(Lr(wt)(g)(K(E))([Q(Go)(bo.value)("range"),Q(rf)(Ei.value)("0"),Q(ef)(gi.value)("100"),Q(af)(Si.value)("1"),Q(uf)(hi.value)("50"),Q(Gk)(Ht.value)("width: 100%;"),Q(nf)(Ci.value)(rr(function(){var i=mr(tt)(ur)(Jf(Ge)(qf)(n.slider)),l=zn(Ea)(qc);return function(p){return i(l(Xc(p)))}}()))]))(ae),An($e(a)(a))(tr(wt)(g)([K(E)(Q(sc)(Ht.value)("width:100%; padding:1.0rem;")),Lr(wt)(g)(_(A)(function(){var i=Q(dr)(sr.value);return function(l){return i(rr(S(l)))}}()))([j(A)(u.startStop.loading)(L(tt)(void 0)),ar(A)(u.startStop.stop)(function(i){return H(J)(i)(H(J)(t(L(tt)(void 0)))(n.startStop.start(void 0)))}),ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(i){return i.value0})(r)))(j(A)(o)(Y(Z))))(function(i){return function(){i(),n.startStop.loading(void 0)();var p=ge(z.value)(),s=vo(nt(Ir)(Kn(Or))(function(m){return nt(Ir)(Ru(Or)(m))(function(v){return nt(Ir)(_(Di)(Mh())(dC($n)(wh)(Et(m))(Ph()(cL))))(function(f){return nt(Ir)(vr(Or)(Xl(0)(5e4)))(function(b){var ct=Pp(Dv(ga(PA(f.pluck0))(ic(Gm(Bm()(f))))))({newSeed:wp(b),size:4});return vr(Or)(function(){var Qt=Jn(Yo)(tt)(function(it){return function(){var be=Ka(),qt=Ka();return{x:be,y:qt}}})(sn(0)(127))(),ee=st(m)(c(m)(ct)(p))(),xe=gt(Yi)(function(it){return function(){var be=Me(p)();return aa(tt)(ur)(be)(function(qt){return function(){var Rr=S_(qt)(),da=_(C)(function(){var Un=gl(Qt),ba=_(Pe)(function(pn){return function(Bo){return Bo/255}(pn)});return function(pn){return Un(ba(pn))}}())(ys(bs)(Rr))();return n.canvas(da)(),void 0}})()}})(),Hr=H(J)(H(J)(H(J)(ee)(v))(kn(fr)(m)))(xe);return n.startStop.stop(Hr)(),Hr})})})})}))();return t(function(){return n.startStop.start(void 0)(),Lo(bi(s))()})(),void 0}})])]))([un(tr(wt)(g)([_(A)(S("Turn off"))(u.startStop.stop),_(A)(S("Turn on"))(o),_(A)(S("Loading..."))(u.startStop.loading)]))])]))}}))})}}};var _L=function(){return D.value}(),qh=function(t){return function(e){return function(r){return function(n){var u=Yn(t)(r);return $t({reflectType:function(){return`<div>
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
</div>`}})()()(M()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"ex"}})({reflectSymbol:function(){return"ex"}}))(_L)({next:Oa(R)(E)(n)(H(J)(e(Nf.value))(_n)),ex:O(N(a)(a))(Wh(u)(e)(n))})}}}};var sL=function(){return D.value}(),Gh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
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
</div>`}})()()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(sL)({next:K(E)(Q(dr)(sr.value)(rr(S(H(J)(e(ls.value))(_n)))))})}}}};var vL=function(){return D.value}(),Bh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
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
`}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(vL)({txt:O(N(a)(a))(cr(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ bang
                  $ playbackRate
                  $ AudioEnvelope
                      { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                      , o: 1.5
                      , d: 30.0
                      }
              , delay 3000 (bang (playbackRate (AudioCancel { o: 3.5 })))
              ]
          )
      ]
  ]`)),cancel:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(u)(tr(wt)(g)([lt(),Wu(1e3)(Tt(Zn()(xn)({p:ra(Vo)(j(Pe)(sn(0)(60))([1,1.2,1,.8])),o:1.5,d:30}))),Wu(3e3)(Tt(Zn()(sE)({o:3.5})))]))])])}}))})}}};var dL=function(){return D.value}(),Uh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Envelope</h2>
  <p>The <code>AudioEnvelope</code> parameter corresponds to the Web Audio API's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setValueCurveAtTime"><code>setValueCurveAtTime</code></a> function and sets an envelope <code>p</code> over the duration <code>d</code> starting at time <code>o</code>.</p>
  <pre><code>~txt~</code></pre>
  ~envelope~
  </section>
`}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))(dL)({txt:O(N(a)(a))(cr(`\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ bang
                  $ playbackRate
                  $ AudioEnvelope
                      { p: join (0 .. 60 $> [ 1.0, 1.2, 1.0, 0.8 ])
                      , o: 1.5
                      , d: 30.0
                      }
              ]
          )
      ]
  ]`)),envelope:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(u)(tr(wt)(g)([lt(),Wu(1e3)(Tt(Zn()(xn)({p:ra(Vo)(j(Pe)(sn(0)(60))([1,1.2,1,.8])),o:1.5,d:30})))]))])])}}))})}}};var yL=function(){return D.value}(),Hh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
  <h2>AudioNumeric</h2>
  <p><code>AudioNumeric encompasses the following three functions from the Web Audio API</code>:</p>

  <p>Let's explore all of them in the example below.</p>

  <blockquote>Pro tip: When using <code>AudioNumeric</code>, consider starting with a <code>_step</code> transition. Otherwise, the transition may be abrupt and unpleasant!</blockquote>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1000
                  $ oneOf
                      [ bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.0, o: 1.0, t: _step }
                      , bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.3, o: 2.0, t: _linear }
                      ]
              , delay 2500
                  $ oneOf
                      [ bang
                          $ playbackRate
                          $ AudioNumeric { n: 1.0, o: 2.5, t: _step }
                      , bang
                          $ playbackRate
                          $ AudioNumeric { n: 0.7, o: 3.5, t: _exponential }
                      ]
              ]
          )
      ]
  ]</code></pre>

  @numericEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"numericEx"}})({reflectSymbol:function(){return"numericEx"}}))(D.value)(yL)({numericEx:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(u)(tr(wt)(g)([lt(),Wu(1e3)(tr(wt)(g)([Tt(Zn()(Wr)({n:1,o:1,t:nD})),Tt(Zn()(Wr)({n:1.3,o:2,t:Ro}))])),Wu(2500)(tr(wt)(g)([Tt(Zn()(Wr)({n:1,o:2.5,t:nD})),Tt(Zn()(Wr)({n:.7,o:3.5,t:pE}))]))]))])])}}))})}}};var kL=function(){return D.value}(),zh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
  <h2>AudioSudden</h2>
  <p>The simplest change you can make is scheduling a value to change <i>now</i>. This is done with <code>AudioSudden</code>, which is a wrapper around the setter for an audio parameter's <a href="https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/value">\`value\`</a> field in the Web Audio API.</p>

  <p>In the example below, we change a value after it has run for 1.5 seconds.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ gain_ 1.0
      [ loopBuf buf
          ( oneOf
              [ bangOn
              , delay 1500
                  $ bang
                  $ playbackRate
                  $ AudioSudden { n: 1.4 }
              ]
          )
      ]
  ]</code></pre>

  @suddenEx@
  </section>
`}})({reflectType:function(){return"@"}})()()(M()(X)({reflectType:function(){return"suddenEx"}})({reflectSymbol:function(){return"suddenEx"}}))(D.value)(kL)({suddenEx:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(u)(tr(wt)(g)([lt(),Wu(1500)(Tt(Zn()(lE)({n:1.4})))]))])])}}))})}}};var EL=function(){return D.value}(),Vh=function(t){return function(e){return function(r){return $t({reflectType:function(){return`<section>
  <h2>Audio Units</h2>
  <p>In my humble opinion, the summit of Web Audio programming is when audio units control the audio parameters of other audio units. This allows for a form of radical experimentation that is difficult in many other frameworks.</p>

  <p>To control an audio parameter with an audio unit, use the <code>AudioUnit</code> constructor. You can also use a <code>Node D1 l p</code>. If your node is for an arbitrary number of channels, make sure to coerce it to mono using the <code>c1</code> function, as in the example below.</p>

  <pre><code>\\ctx buf -> run2 ctx
  [ loopBuf buf
      ( oneOf
          [ bangOn
          , bang
              $ playbackRate
              $ c1
                  ( gain_ 1.0
                      [ constant 1.0 bangOn
                      , gain_ 0.2 (lowpass_ 100.0 (squareOsc 50.0 bangOn))
                      ]
                  )
          ]
      )
  ]
</code></pre>

  ~unitEx~
  </section>
`}})()()(M()(X)({reflectType:function(){return"unitEx"}})({reflectSymbol:function(){return"unitEx"}}))(EL)({unitEx:O(et(a)(a))(xt(r)(t)(function(n){return Et(n)("https://freesound.org/data/previews/320/320873_527080-hq.mp3")})(function(n){return function(u){return st(n)([ue(Ce)(u)(tr(wt)(g)([lt(),Tt(Zn()(iE(a)(a))(oE(Rt(ot)(W(a)(a)(a))(1)([cs(ts)(1)(lt()),Rt(ot)(Qe(a)(a)(a))(.2)(wc(rs)(Qe(a)(a)(a))(100)(v_(bc)(50)(lt())))]))))]))])}}))})}}};var SL=function(){return D.value}(),Jh=function(t){return function(e){return function(r){return function(n){var u=H(J)(e(E_.value))(_n),o=Yn(t)(r);return $t({reflectType:function(){return`<div>
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
</div>`}})()()(M()(M()(on()(M()(M()(M()(X)({reflectType:function(){return"unit"}})({reflectSymbol:function(){return"unit"}}))({reflectType:function(){return"sudden"}})({reflectSymbol:function(){return"sudden"}}))({reflectType:function(){return"numeric"}})({reflectSymbol:function(){return"numeric"}}))({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))({reflectType:function(){return"envelope"}})({reflectSymbol:function(){return"envelope"}}))({reflectType:function(){return"cancel"}})({reflectSymbol:function(){return"cancel"}}))(SL)({sudden:O(N(a)(a))(zh(o)(e)(n)),numeric:O(N(a)(a))(Hh(o)(e)(n)),envelope:O(N(a)(a))(Uh(o)(e)(n)),cancel:O(N(a)(a))(Bh(o)(e)(n)),unit:O(N(a)(a))(Vh(o)(e)(n)),next:Oa(R)(E)(n)(u)})}}}};var TL=function(){return D.value}(),jh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
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
</div>`}})()()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(TL)({next:K(E)(Q(dr)(sr.value)(rr(S(H(J)(e(_s.value))(_n)))))})}}}};var FL=function(){return D.value}(),Xh=function(t){return function(e){return function(r){return function(n){return $t({reflectType:function(){return`<div>
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
</div>`}})()()(on()(X)({reflectType:function(){return"next"}})({reflectSymbol:function(){return"next"}}))(FL)({next:K(E)(Q(dr)(sr.value)(rr(S(H(J)(e(Lf.value))(_n)))))})}}}};var wL=function(){return D.value}(),Qh=function(t){return function(e){return function(r){return function(n){return le({reflectType:function(){return`<div>
  <h1>Imperative API</h1>

  <h2>Like JavaScript, but PureScript</h2>
  <p>
    If you're coming from the JavaScript or TypeScript world, or if you're a fan of monadic <code>do</code> notation, you may enjoy building things step-by-step rather than constructing large declarative structures. If you're that sort of person, this section is for you!
  </p>

  <h2>Parting shot</h2>
  <p>Thanks for checking out wags! We want it to be the most ergonomimc, expressive, and performant Web Audio API on your side of the Mississippi. It certainly is for me, and as I'm in Finland, I'm on <i>both sides</i> of the Mississippi, so you can't beat that! If you have any questions, comments, concerns or would just like to say "hi!", please check out the <a href="https://github.com/mikesol/purescript-wags">Wags GitHub Repo</a> or the <a href="https://purescript.org/chat">PureScript Discord's music channel</a>. Happy wagging!</p>
</div>`}})({reflectType:function(){return"~"}})()()(X)(D.value)(wL)({})}}}};var ML=`module Main where

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
import WAGS.Core (bangOn)
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
`,OL=au(function(t){return It(function(e){return gt(t)(function(r){return function(){var u=Ka();return e(r(u))()}})})}),IL=function(){return D.value}(),RL="https://freesound.org/data/previews/339/339810_5121236-lq.mp3",Kh=function(t){return function(e){return function(r){return le({reflectType:function(){return`<section>
  <h2>Hello subgraph</h2>

  <p>Subgraphs have the type <code>Event (Event (Channel outputChannels lock payload))</code>. Streaming audio is a data type with two constructors: <code>Sound (Node outputChannels lock payload)</code> to create a subgraph and <code>Silence</code> to turn it off. The inner event listens for sound/silence, and the outer event adds subgraphs to the scene. You can create as many subgraphs as you like: wags automatically frees up resources when you send the <code>Silence</code> event. Note that, once you turn a subraph off with <code>Silence</code>, you can't turn it back on again. In this case, just create a new subgraph.</p>

  <p>Here's a simple subgraph that is connected to a slider. As you slide the slider, new nodes are provisioned. Each one has a pseudo-random pitch.</p>

  <pre><code>@txt@</code></pre>
  @ex1@

</section>
`}})({reflectType:function(){return"@"}})()()(M()(M()(X)({reflectType:function(){return"txt"}})({reflectSymbol:function(){return"txt"}}))({reflectType:function(){return"ex1"}})({reflectSymbol:function(){return"ex1"}}))(D.value)(IL)({txt:O(N(a)(a))(cr(ML)),ex1:O(et(a)(a))(Nu()(_r({reflectSymbol:function(){return"slider"}})()()(Lu({reflectSymbol:function(){return"startStop"}})()()()(_r({reflectSymbol:function(){return"loading"}})()()(_r({reflectSymbol:function(){return"start"}})()()(_r({reflectSymbol:function(){return"stop"}})()()(Bn)()()()())()()()())()()()())(Bn)()()()())()()()())(D.value)(function(n){return function(u){var o=P(R)(K(E)(void 0))(u.startStop.start),c=ki(E)(rt.create)(OL)(Pu(E)(function(l){return function(p){return p+1|0}})(u.slider)(0)),i=function(l){return[Rt(ot)(nE(a)(a)(a))(1)(_(A)(function(p){return tr(wt)(g)([K(E)(new eD(Gn(p_(kt(At()(U(U(yt)(AE)()()()({reflectSymbol:function(){return"playbackRate"}}))(c_)()()()({reflectSymbol:function(){return"buffer"}})))(bt()())))({buffer:l,playbackRate:.7+Ga(p)*2})(lt()))),Wu(5e3)(K(E)(rD.value))])})(c))]};return F(N(a)(a))(Ue($e(a)(a))([Ue($e(a)(a))([cr("Slide me!"),yo(N(a)(a))(Lr(wt)(g)(K(E))([Q(Go)(bo.value)("range"),Q(rf)(Ei.value)("0"),Q(ef)(gi.value)("100"),Q(af)(Si.value)("1"),Q(uf)(hi.value)("50"),Q(nf)(Ci.value)(rr(S(n.slider(void 0))))]))(ae)]),An($e(a)(a))(Lr(wt)(g)(_(A)(function(){var l=Q(dr)(sr.value);return function(p){return l(rr(S(p)))}}()))([j(A)(u.startStop.loading)(L(tt)(void 0)),ar(A)(u.startStop.stop)(function(l){return H(J)(l)(H(J)(t(L(tt)(void 0)))(n.startStop.start(void 0)))}),ar(A)(Tn(E)(P(R)(K(E)(L(tt)(void 0)))(_(A)(function(l){return l.value0})(r)))(j(A)(o)(Y(Z))))(function(l){return function(){l(),n.startStop.loading(void 0)();var s=vo(nt(Ir)(Kn(Or))(function(m){return nt(Ir)(Ru(Or)(m))(function(v){return nt(Ir)(Et(m)(RL))(function(f){return vr(Or)(function(){var ct=ds(i(f))(),vt=H(J)(H(J)(ct)(v))(kn(fr)(m));return n.startStop.stop(vt)(),vt})})})}))();return t(function(){return n.startStop.start(void 0)(),Lo(bi(s))()})(),void 0}})]))([un(tr(wt)(g)([_(A)(S("Turn off"))(u.startStop.stop),_(A)(S("Turn on"))(o)]))])]))}}))})}}};var LL=function(){return D.value}(),Yh=function(t){return function(e){return function(r){return function(n){var u=Yn(t)(r);return $t({reflectType:function(){return`<div>
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
</div>`}})()()(M()(M()(X)({reflectType:function(){return"suby"}})({reflectSymbol:function(){return"suby"}}))({reflectType:function(){return"appl"}})({reflectSymbol:function(){return"appl"}}))(LL)({appl:O(et(a)(a))(Ds("\u{1F44F}")(n)(u)(function(o){return Et(o)("https://freesound.org/data/previews/277/277021_1402315-lq.mp3")})(function(o){return function(c){return st(o)([Rt(ot)(W(a)(a)(a))(1)([ue(Ce)(c)(lt())])])}})),suby:O(N(a)(a))(Kh(u)(e)(n))})}}}};var K1t=function(t){return t},Y1t={Coercible0:function(){}},qL=function(t){return function(e){var r=function(u){var o=function(c){if(c instanceof y_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(qh(u.setCancellation)(u.setPage))));if(c instanceof Nf)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an($h(u.setCancellation)(u.setPage))));if(c instanceof k_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Th(u.setCancellation)(u.setPage))));if(c instanceof A_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(SS(u.setCancellation)(u.setPage))));if(c instanceof ls)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Xh(u.setCancellation)(u.setPage))));if(c instanceof Lf)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(yh(u.setCancellation)(u.setPage))));if(c instanceof g_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Jh(u.setCancellation)(u.setPage))));if(c instanceof E_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(lh(u.setCancellation)(u.setPage))));if(c instanceof _s)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Qh(u.setCancellation)(u.setPage))));if(c instanceof $C)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Gh(u.setCancellation)(u.setPage))));if(c instanceof C_)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(Yh(u.setCancellation)(u.setPage))));if(c instanceof wC)return Ue(et(a)(a))(_(A)(F(N(a)(a)))(an(jh(u.setCancellation)(u.setPage))));throw new Error("Failed pattern match at WAGS.Example.Docs (line 143, column 5 - line 143, column 78): "+[c.constructor.name])};return o(u.page)},n=Pu(E)(function(u){if(u instanceof Nc)return function(o){return{prevPage:new B(o.curPage),curPage:u.value0,cancel:o.cancel,pageChange:!0}};if(u instanceof PD)return function(o){return{cancel:u.value0,pageChange:!1,curPage:o.curPage,prevPage:o.prevPage}};throw new Error("Failed pattern match at WAGS.Example.Docs (line 133, column 7 - line 135, column 75): "+[u.constructor.name])})(e)({prevPage:z.value,curPage:y_.value,cancel:L(tt)(void 0),pageChange:!0});return[Ue($e(a)(a))(_(Pe)(function(u){return Fv($e(a)(a))([xv($e(a)(a))(P(R)(Lr(wt)(g)(K(E))([Q(dr)(sr.value)(rr(S(t(new Nc(u.value0))))),Q(Uk)(Ht.value)("cursor:pointer;")]))(_(A)(function(o){return Q(dr)(sr.value)(rr(S(function(){return o.cancel(),t(new Nc(u.value0))()})))})($l(du)(function(){var o=lu(Wa);return function(c){return o(function(i){return i.pageChange}(c))}}())(n))))([cr(u.value1.value0)]),vc($e(a)(a))(K(E)(Q(Up)(Ht.value)(function(){return u.value1.value1?"":"display:none;"}())))([cr(" | ")])])})([new rt(y_.value,new rt("Home",!0)),new rt(Nf.value,new rt("Hello world",!0)),new rt(k_.value,new rt("Array, fan, and fix",!0)),new rt(A_.value,new rt("Audio units",!0)),new rt(Lf.value,new rt("Events",!0)),new rt(g_.value,new rt("Parameters",!0)),new rt(E_.value,new rt("State",!0)),new rt(C_.value,new rt("Subgraphs",!1))])),Ue(Wk(a)(a))(Lk(function(u){return r({page:u.curPage,setPage:function(o){return t(Nc.create(o))},setCancellation:function(o){return t(PD.create(o))}})})($l(du)(function(u){return u.pageChange})(n)))]}},Z1t=function(t){return{page:t,setPage:De(Pi(So(Xo))),setCancellation:De(Pi(So(Xo)))}},tLt=function(){var e=nt(Ge)(nt(Ge)(Ai)(Jd))(zC)();return aa(tt)(ur)(_(wr)(VC)(e))(function(r){return function(){var u=wv(),o=e_(),c=qk(r)(qL(o.push)(o.event))(ng);return Le(C)(gt(c)(function(i){return i(u)}))(),o.push(new Nc(y_.value))()}})()};export{K1t as TopLevelSg,tLt as main,Y1t as newtypeTopLevelSg_,Z1t as p2tl,qL as scene};
