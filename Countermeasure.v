`timescale 1ns / 1ps
//82 clocks - dividing the 16 SBoxes into 2
            //diving the area consumbed by SBOX by a factor of 4 or 25%
          //  might give significant reduction in area and power
module AES_Composite_enc(Kin, Din, Dout, Krdy, Drdy, Kvld, Dvld, EN, BSY, CLK, RSTn, cc_out); //Successful design on hardware

   input  [127:0] Kin;  // Key input
   input [127:0]  Din;  // Data input
   output [127:0] Dout; // Data output
   input          Krdy; // Key input ready
   input          Drdy; // Data input ready
   output         Kvld; // Data output valid
   output         Dvld; // Data output valid
   
   input          EN;   // AES circuit enable
   output         BSY;  // Busy signal
   input          CLK;  // System clock
   input          RSTn; // Reset (Low active)

   reg [127:0]    dat,key, rkey;
   reg [15:0]      dat0,dat1,dat2,dat3,dat4,dat5,dat6,dat7;   
   wire [127:0]   dat_next,rkey_next;
   wire [15:0] dat_next0;
   reg [79:0]      rnd;  
   reg [7:0]      rcon; 
   reg            sel;  // Indicate final round
   reg            Dvld, Kvld, BSY;
   wire           rst;
	reg [79:0]      store;
   reg            m;
   reg [1:0]      n;
   reg [2:0]      k;
   reg [3:0]      l;
   reg [3:0] d;

   reg [7:0] i; //counter
   
   //===================
   (* keep = "true" *) (* s = "true" *) output reg [31:0] cc_out;
   reg [31:0] R;
   wire [31:0] r;
   wire [31:0] b;
	reg [3:0] j;
	reg on;
   //===================

assign rst = ~RSTn;

//Instantiations
aes_core dut1(sel,i,dat,dat0,dat1,dat2,dat3,dat4,dat5,dat6,rkey_next,dat_next,dat_next0);//removed dat7 (redundant)
KeyExpantion keyexpantion (.kin(rkey), .kout(rkey_next), .rcon(rcon));


always @(posedge CLK or posedge rst) begin
      if (rst)     Dvld <= 0;
      else if (EN) Dvld <= sel;
   end

   always @(posedge CLK or posedge rst) begin
      if (rst) Kvld <= 0;
      else if (EN) Kvld <= Krdy;
   end

   always @(posedge CLK or posedge rst) begin
      if (rst) BSY <= 0;
      else if (EN) BSY <= Drdy || (rnd[79:1] | sel);
   end  
   
   /*always @(posedge CLK or posedge rst) begin
      if (rst)             rnd <= 80'd1;
      else if (EN) begin
       if(Drdy)          rnd <= {rnd[78:0],rnd[79]};
       else if(~rnd[0])  rnd <= {rnd[78:0],rnd[79]};
      end
      end*/
      
 always@(posedge CLK or posedge rst)
 begin
  if(rst) i<=8'd0; 
  else if (EN) begin
       if(i[7])         i<=8'd1;
       else if(Drdy)    i<=8'd1;
       else if(~rnd[0]) i<={i[6:0],i[7]};
 end
 end
   
 always @(posedge CLK or posedge rst) begin
      if (rst)     sel <= 0;
      else if (EN) sel <= rnd[79];
   end
 
 //Input 
 always @(posedge CLK or posedge rst) begin
      if (rst)
      begin
              dat <= 128'h0;
              dat7<=16'h0;
      end
      else if (EN) begin
         if (Drdy)   
         begin            // dat <= Din ^ key;
         dat <= Din ^ Kin;
         end
         else if ((i[7]) && (~rnd[0]|sel)) 
            begin
            dat <= dat_next;   
            dat7<=dat_next0;   
            end
        // else if(Drdy)  dat3<=dat[31:0]; 
      end
   end
   
 always @(posedge CLK or posedge rst) begin
      if (rst)                 dat0 <= 16'h0;
      else if (EN) begin
          if ((i[0]) && (~rnd[0]|sel))  dat0 <= dat_next0;   
          //else if(Drdy)  dat0<=dat[127:96]; 
      end
   end
  
 always @(posedge CLK or posedge rst) begin
      if (rst)                 dat1 <= 16'h0;
      else if (EN) begin
          if ((i[1]) && (~rnd[0]|sel))  dat1 <= dat_next0; 
          //else if(Drdy)  dat1<=dat[95:64];     
      end
   end
   
 always @(posedge CLK or posedge rst) begin
      if (rst)                 dat2 <= 16'h0;
      else if (EN) begin
          if ((i[2]) && (~rnd[0]|sel))  dat2 <= dat_next0; 
          //else if(Drdy)  dat2<=dat[63:32];     
      end
   end
  always @(posedge CLK or posedge rst) begin
      if (rst)                 dat3 <= 16'h0;
      else if (EN) begin
          if ((i[3]) && (~rnd[0]|sel))  dat3<= dat_next0; 
          //else if(Drdy)  dat2<=dat[63:32];     
      end
   end
always @(posedge CLK or posedge rst) begin
      if (rst)                 dat4 <= 16'h0;
      else if (EN) begin
          if ((i[4]) && (~rnd[0]|sel))  dat4<= dat_next0; 
          //else if(Drdy)  dat2<=dat[63:32];     
      end
   end
always @(posedge CLK or posedge rst) begin
      if (rst)                 dat5 <= 16'h0;
      else if (EN) begin
          if ((i[5]) && (~rnd[0]|sel))  dat5<= dat_next0; 
          //else if(Drdy)  dat2<=dat[63:32];     
      end
   end
always @(posedge CLK or posedge rst) begin
      if (rst)                 dat6 <= 16'h0;
      else if (EN) begin
          if ((i[6]) && (~rnd[0]|sel))  dat6<= dat_next0; 
          //else if(Drdy)  dat2<=dat[63:32];     
      end
   end
   
   
 assign Dout = dat; 

 always @(posedge CLK or posedge rst) begin
      if (rst)     key <= 128'h0;
      else if (EN)
        if (Krdy)  key <= Kin;
   end
   
   //Key
   always @(posedge CLK or posedge rst) begin
      if (rst)         rkey <= 128'h0;
      else if (EN) begin
         if (Krdy)        rkey <= Kin;
         else if (rnd[0] && ~on) rkey <= key;
         else if (i[7] && (~rnd[0]|sel)) rkey <= rkey_next;
      end
   end 
  
 always @(posedge CLK or posedge rst) begin
     if (rst)          rcon <= 8'h01;
     else if (EN) begin
        if (Drdy)    rcon <= 8'h01;
        else if ((i[7])&& ~rnd[0]) rcon <= xtime(rcon);
     end
   end
   
   function [7:0] xtime;
      input [7:0] x;
      xtime = (x[7]==1'b0)? {x[6:0],1'b0} : {x[6:0],1'b0} ^ 8'h1B;
   endfunction
	
	//false clock insertion with collatz conjecture (rnd[8] | rnd[72]){important code for first and last round insertion}
	
	always @(posedge CLK or posedge rst) begin
      if (rst)                 m <= 1'b0;
		else if (m!=1'b0)           m <= 1'b0;
      else if (EN && (~store[0]) && (rnd[8] | rnd[72]) && d[0])   m <= 1'b1;
      else m <= 1'b0;
   end
   
   always @(posedge CLK or posedge rst) begin
      if (rst)                 n <= 2'b00;
		else if (n!=2'd0)           n <= {1'b0,n[1]};
      else if (EN && (~store[0]) && (rnd[8] | rnd[72]) && d[1])   n <= 2'b10;
		else n <= 2'd0;
   end
	
   always @(posedge CLK or posedge rst) begin
      if (rst)                 k <= 3'b000;
		else if (k!=3'd0)           k <= {1'b0,k[2:1]};    
      else if (EN && (~store[0]) && (rnd[8] | rnd[72]) && d[2])   k <= 3'b100;
		else k <= 3'd0;
   end
   
   always @(posedge CLK or posedge rst) begin
      if (rst)                 l <= 4'b0000;
		else if (l!=4'd0)           l <= {1'b0,l[3:1]};
      else if (EN && (~store[0]) && (rnd[8] | rnd[72]) && d[2])   l <= 4'b1000;
		else l <= 4'd0;
   end
	
	
	
	always @ (posedge CLK or posedge rst) begin
	   if (rst) on <= 1'b0;
		else if (rnd[8] | rnd[72] && ~store[0]) on <= 1'b1;
		else if ((!m) && (!n) && (!k) && (!l)) on <= 1'b0;
	end
	
	wire h = (on && (!m) && (!n) && (!k) && (!l));
	
	always @(posedge CLK or posedge rst) begin
      if (rst)    begin    rnd <= 80'd1;  store <= 80'd1; end
      else if (EN) begin
         if (Drdy)  begin       rnd <= {rnd[78:0], rnd[79]};  store <= 80'd0; end
         else if ((rnd[8] | rnd[72] && ~store[0]) || h)  begin 
                 if(store==80'd0) begin store <= {rnd[78:0], rnd[79]}; rnd <= 80'd1;  end
                 else if ((!m) && (!n) && (!k) && (!l)) begin rnd <= store; store <= 80'd0; end    
         end
			else if(~rnd[0])  rnd <= {rnd[78:0],rnd[79]};
      end
   end
   
   //Collatz conjecture---------------------------------
   
   assign r = R;
   assign b = cc_out;
   
   always @ (posedge CLK or posedge rst) begin
   //if (rst) R <= 8'hff;
	//if (rst) R <= 16'hffff;
   if (rst) R <= 32'hffffffff;
	//if (rst) R <= 64'hffffffffffffffff;
	//if (rst) R <= 128'hffffffffffffffffffffffffffffffff;
	else R <= cc_out;
   end
   
   /*always @ (posedge CLK or posedge rst) begin
     if (rst) cc_out <= R;
     else if (!(!(store ^ rnd)) && !(rnd^10'b00_0000_0001)) begin
       case (dat[1:0]%4)
         2'd0: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<1)+r[7:0]+8'd1;
         2'd1: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+8'd1;
         2'd2: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+r[7:0]+8'd1;
         2'd3: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+(r[7:0]<<1)+8'd1;
         default: cc_out <= b;
       endcase
     end
     else cc_out <= b;
   end*/
    
    always @ (posedge CLK or posedge rst) begin
      if (rst) j <= 4'd0;
		else if (sel) j <= 4'd0;
      else if (i[7]) j <= j + 4'd1;
      else j <= j;
    end
	wire g = ((|rnd[8:0]) || (j == 4'd9)) && BSY;
	//2 rounds only
	always @ (*) begin
     if (rst) cc_out <= R;
     else if (g) begin //!(rnd^10'd2) || sel <====code1, (Drdy && !(rnd^10'd1)) || sel <====code2
       /*case (dat_next0[1:0]%4)
         2'd0: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<1)+r[7:0]+8'd1;
         2'd1: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+8'd1;
         2'd2: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+r[7:0]+8'd1;
         2'd3: cc_out[7:0] <= (r[0]==0)?(r[7:0]>>1):(r[7:0]<<2)+(r[7:0]<<1)+8'd1;
         default: cc_out <= b;
       endcase*/
		 /*case (dat_next0[1:0]%4)
         2'd0: cc_out[15:0] <= (r[0]==0)?(r[15:0]>>1):(r[15:0]<<1)+r[15:0]+16'd1;
         2'd1: cc_out[15:0] <= (r[0]==0)?(r[15:0]>>1):(r[15:0]<<2)+16'd1;
         2'd2: cc_out[15:0] <= (r[0]==0)?(r[15:0]>>1):(r[15:0]<<2)+r[15:0]+16'd1;
         2'd3: cc_out[15:0] <= (r[0]==0)?(r[15:0]>>1):(r[15:0]<<2)+(r[15:0]<<1)+16'd1;
         default: cc_out <= b;
       endcase*/
		 case (dat_next0[1:0]%4)
         2'd0: cc_out[31:0] <= (r[0]==0)?(r[31:0]>>1):(r[31:0]<<1)+r[31:0]+32'd1;
         2'd1: cc_out[31:0] <= (r[0]==0)?(r[31:0]>>1):(r[31:0]<<2)+32'd1;
         2'd2: cc_out[31:0] <= (r[0]==0)?(r[31:0]>>1):(r[31:0]<<2)+r[31:0]+32'd1;
         2'd3: cc_out[31:0] <= (r[0]==0)?(r[31:0]>>1):(r[31:0]<<2)+(r[31:0]<<1)+32'd1;
         default: cc_out <= b;
       endcase
		 /*case (dat_next0[1:0]%4)
         2'd0: cc_out[63:0] <= (r[0]==0)?(r[63:0]>>1):(r[63:0]<<1)+r[63:0]+64'd1;
         2'd1: cc_out[63:0] <= (r[0]==0)?(r[63:0]>>1):(r[63:0]<<2)+64'd1;
         2'd2: cc_out[63:0] <= (r[0]==0)?(r[63:0]>>1):(r[63:0]<<2)+r[63:0]+64'd1;
         2'd3: cc_out[63:0] <= (r[0]==0)?(r[63:0]>>1):(r[63:0]<<2)+(r[63:0]<<1)+64'd1;
         default: cc_out <= b;
       endcase*/
		 /*case (dat_next0[1:0]%4)
         2'd0: cc_out[127:0] <= (r[0]==0)?(r[127:0]>>1):(r[127:0]<<1)+r[127:0]+128'd1;
         2'd1: cc_out[127:0] <= (r[0]==0)?(r[127:0]>>1):(r[127:0]<<2)+128'd1;
         2'd2: cc_out[127:0] <= (r[0]==0)?(r[127:0]>>1):(r[127:0]<<2)+r[127:0]+128'd1;
         2'd3: cc_out[127:0] <= (r[0]==0)?(r[127:0]>>1):(r[127:0]<<2)+(r[127:0]<<1)+128'd1;
         default: cc_out <= b;
       endcase*/
     end
     else cc_out <= b;
    end
	 
	 wire [1:0]  p;

    assign p=cc_out%4;
   
    always@(*) begin
     case(p)
     0: begin d=4'b0001; end
     1: begin d=4'b0010; end
     2: begin d=4'b0100; end
     3: begin d=4'b1000; end
     endcase
	 end
 
endmodule

//***********************************************************************************************

module aes_core(sel,i,dat,din0,din1,din2,din3,din4,din5,din6,k,datout,dout);
input sel;
input [7:0]i;
input [15:0] din0,din1,din2,din3,din4,din5,din6;
output reg[15:0] dout;
input [127:0] dat,k;
output [127:0] datout;
reg [15:0] a;
wire [15:0]b; //inputs to byte-substitution block

wire [127:0]data,data1,data2;
wire [127:0] rx; //input to row-shift block
wire [127:0] rs; //output after row-shift
wire [31:0] mx0,mx1,mx2,mx3; //input to mix-column block
wire [31:0] my0,my1,my2,my3; //output of mix-column block

masoleh f(a,b);
always@(*)begin 
    case(i)
    8'd1: begin
        a<=dat[127:112];
        dout<=b;
    end
    8'd2: begin
        a<=dat[111:96];
        dout<=b;
    end
    8'd4: begin
        a<=dat[95:80];
        dout<=b;
    end
    8'd8: begin
        a<=dat[79:64];
        dout<=b;
    end
    8'd16: begin
        a<=dat[63:48];
        dout<=b;
    end
    8'd32: begin
        a<=dat[47:32];
        dout<=b;
    end
    8'd64: begin
        a<=dat[31:16];
        dout<=b;
    end
    8'd128: begin
        a<=dat[15:0];
        dout<=b;
    end
	 default: begin
	     a<=dat[15:0];
        dout<=b;
	 end
    endcase
end
//ROW-SHIFT MODULE

row_shift f17(rx,rs);

//MIX-COLUMN MODULE
MixColumns f18(mx0,my0);
MixColumns f19(mx1,my1);
MixColumns f20(mx2,my2);
MixColumns f21(mx3,my3);
 
 
 assign data={din0,din1,din2,din3,din4,din5,din6,b};
 
 assign rx=data;
 assign data1=rs;
 
 assign {mx0,mx1,mx2,mx3}=data1;
 assign data2={my0,my1,my2,my3};
 
 assign datout=(sel)?data1^k:data2^k;

endmodule

//************************************************************************************************
module MixColumns(x, y);

   //------------------------------------------------
   input  [31:0]  x;
   output [31:0]  y;

   //------------------------------------------------
   wire [7:0]    a0, a1, a2, a3;
   wire [7:0]    b0, b1, b2, b3;

   assign a0 = x[31:24];
   assign a1 = x[23:16];
   assign a2 = x[15: 8];
   assign a3 = x[ 7: 0];

   assign b0 = xtime(a0);
   assign b1 = xtime(a1);
   assign b2 = xtime(a2);
   assign b3 = xtime(a3);

   assign y[31:24] =    b0 ^ a1^b1 ^ a2    ^ a3;
   assign y[23:16] = a0        ^b1 ^ a2^b2 ^ a3;
   assign y[15: 8] = a0    ^ a1        ^b2 ^ a3^b3;
   assign y[ 7: 0] = a0^b0 ^ a1    ^ a2        ^b3;
  
   function [7:0] xtime;
      input [7:0] x;
      xtime = (x[7]==1'b0)? {x[6:0],1'b0} : {x[6:0],1'b0} ^ 8'h1B;
   endfunction
   
endmodule // MixColumns
//************************************************************************************************

module KeyExpantion (kin, kout, rcon);

   //------------------------------------------------
   input [127:0]  kin;
   output [127:0] kout;
   input [7:0] 	  rcon;

   //------------------------------------------------
   wire [31:0]   ws,wr, w0, w1, w2, w3;
   wire [15:0] ws1,ws2;

   //------------------------------------------------
  // maximov SB1 ({kin[23:16], kin[15:8], kin[7:0], kin[31:24]}, ws);
   masoleh max1({kin[23:16],kin[15:8]},ws1);
   masoleh max2({kin[7:0],kin[31:24]},ws2);
   assign ws={ws1,ws2};
   assign wr = {(ws[31:24] ^ rcon), ws[23:0]};

   assign w0 = wr ^ kin[127:96];
   assign w1 = w0 ^ kin[95:64];
   assign w2 = w1 ^ kin[63:32];
   assign w3 = w2 ^ kin[31:0];

   assign kout = {w0, w1, w2, w3};

endmodule // KeyExpantion

//*********************************************************************************************
///////////masoleh/////////////////

module masoleh(x,y); // maximov based sbox
input [15:0] x;
output [15:0] y;

wire [7:0] byte_in1, byte_in2;
wire [7:0] byte_out1, byte_out2;

assign byte_in1 = x[15:8];
assign byte_in2 = x[7:0];

Newlight_sbox cm1(byte_in1, byte_out1);
Newlight_sbox cm2(byte_in2, byte_out2);

assign y = {byte_out1, byte_out2};
endmodule
module Newlight_sbox( input [7:0] g,
                      output [7:0] s
    );
    
wire y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18;
wire l1,l2,l3,l4;
wire x1,x2,x3,x4;
wire r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12;

In_tf m1 (.g(g),.a0(y1),.a1(y2),.a2(y3),.a3(y4),.b0(y5),.b1(y6),.b2(y7),.b3(y8),
          .a01(y9),.a02(y10),.a13(y11),.a23(y12),.ap(y13),.b01(y14),.b02(y15),.b13(y16),.b23(y17),.bp(y18)   );
          
expo_comp m2 (.a0(y1) ,.a1(y2),.a2(y3),.a3(y4),.b0(y5),.b1(y6),.b2(y7),.b3(y8),
           .a01(y9),.a02(y10),.a13(y11),.a23(y12),.ap(y13),.b01(y14),.b02(y15),.b13(y16),.b23(y17),.bp(y18),.d0(l1),.d1(l2),.d2(l3),.d3(l4));
Inv_4 m3 (   .d0(l1),.d1(l2),.d2(l3),.d3(l4),.e0(x1),.e1(x2),.e2(x3),.e3(x4)  );
mul_out m4 ( .e0(x1),.e1(x2),.e2(x3),.e3(x4),.a0(y1),.a1(y2),.a2(y3),.a3(y4),.b0(y5),.b1(y6),.b2(y7),.b3(y8),
             .a01(y9),.a02(y10),.a13(y11),.a23(y12),.ap(y13),.b01(y14),.b02(y15),.b13(y16),.b23(y17),.bp(y18),
             .w0(r1),.w1(r2),.w2(r3),.w3(r4),.w4(r5),.w5(r6),.z0(r7),.z1(r8),.z2(r9),.z3(r10),.z4(r11),.z5(r12) );
out_tf m5 (  .w0(r1),.w1(r2),.w2(r3),.w3(r4),.w4(r5),.w5(r6),.z0(r7),.z1(r8),.z2(r9),.z3(r10),.z4(r11),.z5(r12),.s(s));            
             
endmodule
module In_tf(input [7:0] g,
             output a0,a1,a2,a3,b0,b1,b2,b3,
             output a01,a02,a13,a23,ap,b01,b02,b13,b23,bp
             );
             wire t0,t1;
assign a0 = b3^g[4];
assign a1 = a13^a3;
assign a2 = b3^g[7];
assign a3 = b3^g[1];

assign b0 = g[0];
assign b1 = b01^g[0];
assign b2 = b0^b02;
assign b3 = b23^b2;

assign a01 = a23^ap;
assign a02 = g[7]^g[4];
assign a13 = g[7]^g[2];
assign a23 = g[7]^g[1];
assign ap = g[4]^g[2];

assign b01 = t0^a01;
assign b02 = t1^g[6];
assign b13 = b02^bp;
assign b23 = t1^g[5];
assign bp = b23^b01;

assign t0 = g[3]^g[2];
assign t1 = t0^g[1]; 

endmodule
module expo_comp( input a0,a1,a2,a3,b0,b1,b2,b3,a01,a02,a13,a23,ap,b01,b02,b13,b23,bp,
                  output d0,d1,d2,d3  );


assign d0 = ~(a01&b01)^(~(a02&b02))^(~(a0&b0))^(~(a13|b13));
assign d1 = ~(a01&b01)^(~(a02|b02))^(~(a1&b1))^(~(ap&bp));
assign d2 = ~(a2|b2)^(~(a13&b13))^(~(a23&b23))^(~(a02&b02));
assign d3 = ~(a3&b3)^(~(ap&bp))^(~(a23|b23))^(~(a02&b02));                 
endmodule
module Inv_4( input d0,d1,d2,d3,
              output e0,e1,e2,e3 );
   assign e0 = d3&(d0^d1) | d2&((~d0)|d3);
   assign e1 = (~d2)&d3&(d0^~d1) | d2&(d1|(~d3));           
   assign e2 = d1&(d2^d3) | d0&((~d2)|d1);   
   assign e3 = (~d0)&d1&(d2^~d3) | d0&(d3 | (~d1));       
endmodule
module mul_out( input a0,a1,a2,a3,b0,b1,b2,b3,a01,a02,a13,a23,ap,b01,b02,b13,b23,bp,
                      e0,e1,e2,e3,
                output w0,w1,w2,w3,w4,w5,
                output z0,z1,z2,z3,z4,z5      
    );
  wire e02,e13;

assign  e02 = e0^e2;
assign  e13 = e1^e3;

assign w0 = ~(b1&e0)^(~(b01&e1)); 
assign w1 = ~(b0&e1)^(~(b01&e0));
assign w2 = ~(b3&e2)^(~(b23&e3));
assign w3 = ~(b2&e3)^(~(b23&e2));
assign w4 = ~(b13&e13)^(~(b02&e02));
assign w5 = ~(bp&e13)^(~(b13&e02));

assign z0 = ~(a1&e0)^(~(a01&e1));
assign z1 = ~(a0&e1)^(~(a01&e0));
assign z2 = ~(a3&e2)^(~(a23&e3));
assign z3 = ~(a2&e3)^(~(a23&e2));
assign z4 = ~(a13&e13)^(~(a02&e02));
assign z5 = ~(ap&e13)^(~(a13&e02));   
endmodule
module out_tf( input w0,w1,w2,w3,w4,w5,
               input z0,z1,z2,z3,z4,z5,
               output [7:0] s
                                         );
 wire t2,t3,t4,t5,t6;                                        
  
 assign t2 = w3^z1^z5;                                        
 assign t3 = w0^w2;
 assign t4 = w3^w5;
 assign t5 = w0^w4;  
 assign t6 = t5^z2;
 
 assign s[7] = t2^w5;
 assign s[6] = t4^~s[4];
 assign s[5] = t6^~z4;
 assign s[4] = t2^w1;
 assign s[3] = t3^s[4];
 assign s[2] = t6^z0^s[7];
 assign s[1] = t5^t4^s[0];
 assign s[0] = ~(t3^z3^z5);
                                      
endmodule



//*********************************************************************************************

module row_shift(sb,rs);
input [127:0] sb;
output reg [127:0] rs;

always@(sb)
begin
             //Bytes-1,5,9,13 have no row shift
       rs[127:120] = sb[127:120]; // byte-1
       rs[119:112] = sb[87:80]; // new byte-2 
       rs[111:104] = sb[47:40]; // new byte-3
       rs[103:96]  = sb[7:0];   // new byte-4
             
       rs[95:88]   = sb[95:88]; // byte-5
       rs[87:80]   = sb[55:48]; // new byte-6
       rs[79:72]   = sb[15:8];  // new byte-7
       rs[71:64]   = sb[103:96];// new byte-8
             
       rs[63:56]   = sb[63:56]; // byte-9
       rs[55:48]   = sb[23:16]; // new byte-10
       rs[47:40]   = sb[111:104];// new byte-11
       rs[39:32]   = sb[71:64]; // new byte-12
             
       rs[31:24]   = sb[31:24]; // byte-13
       rs[23:16]   = sb[119:112]; // new byte-14
       rs[15:8]    = sb[79:72]; // new byte-15
       rs[7:0]     = sb[39:32]; // new byte-16 
end                 
endmodule
