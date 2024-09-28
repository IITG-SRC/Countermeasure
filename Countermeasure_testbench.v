module AES_82clk_mysimu(    );
    reg [127:0] plain_text,key;
    wire [127:0] cipher_text;
    reg clk;
    reg reset,Krdy,Drdy,EN;
    wire Kvld,Dvld;
    integer i;
    wire BSY;
    initial begin
    clk = 1;
    forever 
         #10 clk = ~clk;
    end
    
 // AES_Composite_enc encrypt(.plain_text(plain_text), .cipher_text(cipher_text), .key(key), .clk(clk), .Krdy(Krdy), .Drdy(Drdy), .EN(EN), .Kvld(Kvld), .Dvld(Dvld), .reset(reset));
   AES_Composite_enc enc0(.Kin(key), .Din(plain_text),.Dout(cipher_text), .Krdy(Krdy), .Drdy(Drdy), .Kvld(Kvld), .Dvld(Dvld), .EN(EN), .BSY(BSY), .CLK(clk), .RSTn(reset)); 
   initial
    begin
    plain_text<=128'h00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00;
    key  <=     128'h01_23_45_67_89_ab_cd_ef_12_34_56_78_9a_bc_de_f0;
    reset<=0;
    Krdy<=0;
    Drdy<=0;
    EN<=0;
    #20;
    reset<=1;
    Krdy<=1;
    Drdy<=1;
    EN<=1;
    
    #20;
    Drdy<=0;
    Krdy<=0;
    #1700;
     
    for(i=0;i<9;i=i+1)
    begin
      plain_text<=cipher_text;
    Krdy<=0;
    //Drdy<=0;
    //EN<=0;
    //#20;
    Krdy<=1;
    Drdy<=1;
    EN<=1;
  
   
    #20;
    Drdy<=0;
    Krdy<=0;
    #1620;
    end
    end
   
   
   /*
   
    initial
    begin
    reset<=1;
    Krdy<=0;
    Drdy<=0;
    EN<=0;
    #20;
    reset<=0;
    Krdy<=1;
    Drdy<=1;
    EN<=1;
    plain_text<=128'h00_00_00_00_00_00_00_00_00_00_00_00_00_00_00_00;
    key  <=     128'h01_23_45_67_89_ab_cd_ef_12_34_56_78_9a_bc_de_f0;
    #20;
    Drdy<=0;
    Krdy<=0;
    #220;
    
    for(i=0;i<9;i=i+1)
    begin
    reset<=0;
    Krdy<=1;
    Drdy<=1;
    EN<=1;
    plain_text<=cipher_text;
    key  <=     128'h01_23_45_67_89_ab_cd_ef_12_34_56_78_9a_bc_de_f0;
    #20;
    Drdy<=0;
    Krdy<=0;
    #220;
    end
    end
    */

endmodule