??$
??
B
AddV2
x"T
y"T
z"T"
Ttype:
2	??
B
AssignVariableOp
resource
value"dtype"
dtypetype?
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
^
Fill
dims"
index_type

value"T
output"T"	
Ttype"

index_typetype0:
2	
.
Identity

input"T
output"T"	
Ttype
:
Less
x"T
y"T
z
"
Ttype:
2	
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(?
=
Mul
x"T
y"T
z"T"
Ttype:
2	?

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:
@
ReadVariableOp
resource
value"dtype"
dtypetype?
E
Relu
features"T
activations"T"
Ttype:
2	
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0?
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
0
Sigmoid
x"T
y"T"
Ttype:

2
[
Split
	split_dim

value"T
output"T*	num_split"
	num_splitint(0"	
Ttype
?
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ?
@
StaticRegexFullMatch	
input

output
"
patternstring
?
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
?
TensorListFromTensor
tensor"element_dtype
element_shape"
shape_type
output_handle"
element_dtypetype"

shape_typetype:
2	
?
TensorListReserve
element_shape"
shape_type
num_elements

handle"
element_dtypetype"

shape_typetype:
2	
?
TensorListStack
input_handle
element_shape
tensor"element_dtype"
element_dtypetype" 
num_elementsint?????????
P
	Transpose
x"T
perm"Tperm
y"T"	
Ttype"
Tpermtype0:
2	
?
VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 ?
?
While

input2T
output2T"
T
list(type)("
condfunc"
bodyfunc" 
output_shapeslist(shape)
 "
parallel_iterationsint
?"serve*2.4.02v2.4.0-rc4-71-g582c8d236cb8??"
z
dense_18/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:* 
shared_namedense_18/kernel
s
#dense_18/kernel/Read/ReadVariableOpReadVariableOpdense_18/kernel*
_output_shapes

:*
dtype0
r
dense_18/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namedense_18/bias
k
!dense_18/bias/Read/ReadVariableOpReadVariableOpdense_18/bias*
_output_shapes
:*
dtype0
l
RMSprop/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_nameRMSprop/iter
e
 RMSprop/iter/Read/ReadVariableOpReadVariableOpRMSprop/iter*
_output_shapes
: *
dtype0	
n
RMSprop/decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/decay
g
!RMSprop/decay/Read/ReadVariableOpReadVariableOpRMSprop/decay*
_output_shapes
: *
dtype0
~
RMSprop/learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *&
shared_nameRMSprop/learning_rate
w
)RMSprop/learning_rate/Read/ReadVariableOpReadVariableOpRMSprop/learning_rate*
_output_shapes
: *
dtype0
t
RMSprop/momentumVarHandleOp*
_output_shapes
: *
dtype0*
shape: *!
shared_nameRMSprop/momentum
m
$RMSprop/momentum/Read/ReadVariableOpReadVariableOpRMSprop/momentum*
_output_shapes
: *
dtype0
j
RMSprop/rhoVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nameRMSprop/rho
c
RMSprop/rho/Read/ReadVariableOpReadVariableOpRMSprop/rho*
_output_shapes
: *
dtype0
?
lstm_6/lstm_cell_17/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*+
shared_namelstm_6/lstm_cell_17/kernel
?
.lstm_6/lstm_cell_17/kernel/Read/ReadVariableOpReadVariableOplstm_6/lstm_cell_17/kernel*
_output_shapes

:<*
dtype0
?
$lstm_6/lstm_cell_17/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*5
shared_name&$lstm_6/lstm_cell_17/recurrent_kernel
?
8lstm_6/lstm_cell_17/recurrent_kernel/Read/ReadVariableOpReadVariableOp$lstm_6/lstm_cell_17/recurrent_kernel*
_output_shapes

:<*
dtype0
?
lstm_6/lstm_cell_17/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*)
shared_namelstm_6/lstm_cell_17/bias
?
,lstm_6/lstm_cell_17/bias/Read/ReadVariableOpReadVariableOplstm_6/lstm_cell_17/bias*
_output_shapes
:<*
dtype0
?
lstm_7/lstm_cell_18/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*+
shared_namelstm_7/lstm_cell_18/kernel
?
.lstm_7/lstm_cell_18/kernel/Read/ReadVariableOpReadVariableOplstm_7/lstm_cell_18/kernel*
_output_shapes

:<*
dtype0
?
$lstm_7/lstm_cell_18/recurrent_kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*5
shared_name&$lstm_7/lstm_cell_18/recurrent_kernel
?
8lstm_7/lstm_cell_18/recurrent_kernel/Read/ReadVariableOpReadVariableOp$lstm_7/lstm_cell_18/recurrent_kernel*
_output_shapes

:<*
dtype0
?
lstm_7/lstm_cell_18/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*)
shared_namelstm_7/lstm_cell_18/bias
?
,lstm_7/lstm_cell_18/bias/Read/ReadVariableOpReadVariableOplstm_7/lstm_cell_18/bias*
_output_shapes
:<*
dtype0
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0
?
RMSprop/dense_18/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:*,
shared_nameRMSprop/dense_18/kernel/rms
?
/RMSprop/dense_18/kernel/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_18/kernel/rms*
_output_shapes

:*
dtype0
?
RMSprop/dense_18/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:**
shared_nameRMSprop/dense_18/bias/rms
?
-RMSprop/dense_18/bias/rms/Read/ReadVariableOpReadVariableOpRMSprop/dense_18/bias/rms*
_output_shapes
:*
dtype0
?
&RMSprop/lstm_6/lstm_cell_17/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*7
shared_name(&RMSprop/lstm_6/lstm_cell_17/kernel/rms
?
:RMSprop/lstm_6/lstm_cell_17/kernel/rms/Read/ReadVariableOpReadVariableOp&RMSprop/lstm_6/lstm_cell_17/kernel/rms*
_output_shapes

:<*
dtype0
?
0RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*A
shared_name20RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms
?
DRMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms/Read/ReadVariableOpReadVariableOp0RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms*
_output_shapes

:<*
dtype0
?
$RMSprop/lstm_6/lstm_cell_17/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*5
shared_name&$RMSprop/lstm_6/lstm_cell_17/bias/rms
?
8RMSprop/lstm_6/lstm_cell_17/bias/rms/Read/ReadVariableOpReadVariableOp$RMSprop/lstm_6/lstm_cell_17/bias/rms*
_output_shapes
:<*
dtype0
?
&RMSprop/lstm_7/lstm_cell_18/kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*7
shared_name(&RMSprop/lstm_7/lstm_cell_18/kernel/rms
?
:RMSprop/lstm_7/lstm_cell_18/kernel/rms/Read/ReadVariableOpReadVariableOp&RMSprop/lstm_7/lstm_cell_18/kernel/rms*
_output_shapes

:<*
dtype0
?
0RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape
:<*A
shared_name20RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms
?
DRMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms/Read/ReadVariableOpReadVariableOp0RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms*
_output_shapes

:<*
dtype0
?
$RMSprop/lstm_7/lstm_cell_18/bias/rmsVarHandleOp*
_output_shapes
: *
dtype0*
shape:<*5
shared_name&$RMSprop/lstm_7/lstm_cell_18/bias/rms
?
8RMSprop/lstm_7/lstm_cell_18/bias/rms/Read/ReadVariableOpReadVariableOp$RMSprop/lstm_7/lstm_cell_18/bias/rms*
_output_shapes
:<*
dtype0

NoOpNoOp
?'
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*?&
value?&B?& B?&
?
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
	variables
regularization_losses
trainable_variables
	keras_api
	
signatures
l

cell

state_spec
	variables
regularization_losses
trainable_variables
	keras_api
l
cell

state_spec
	variables
regularization_losses
trainable_variables
	keras_api
h

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
?
iter
	decay
learning_rate
momentum
 rho	rmsT	rmsU	!rmsV	"rmsW	#rmsX	$rmsY	%rmsZ	&rms[
8
!0
"1
#2
$3
%4
&5
6
7
 
8
!0
"1
#2
$3
%4
&5
6
7
?
'metrics
(layer_metrics
)layer_regularization_losses
	variables
regularization_losses

*layers
trainable_variables
+non_trainable_variables
 
~

!kernel
"recurrent_kernel
#bias
,	variables
-trainable_variables
.regularization_losses
/	keras_api
 

!0
"1
#2
 

!0
"1
#2
?
0metrics
1layer_metrics
2layer_regularization_losses
	variables

3states
regularization_losses

4layers
trainable_variables
5non_trainable_variables
~

$kernel
%recurrent_kernel
&bias
6	variables
7trainable_variables
8regularization_losses
9	keras_api
 

$0
%1
&2
 

$0
%1
&2
?
:metrics
;layer_metrics
<layer_regularization_losses
	variables

=states
regularization_losses

>layers
trainable_variables
?non_trainable_variables
[Y
VARIABLE_VALUEdense_18/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE
WU
VARIABLE_VALUEdense_18/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE

0
1

0
1
 
?
@metrics
Alayer_metrics
Blayer_regularization_losses
	variables
trainable_variables
regularization_losses

Clayers
Dnon_trainable_variables
KI
VARIABLE_VALUERMSprop/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE
MK
VARIABLE_VALUERMSprop/decay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE
][
VARIABLE_VALUERMSprop/learning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE
SQ
VARIABLE_VALUERMSprop/momentum-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUE
IG
VARIABLE_VALUERMSprop/rho(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUElstm_6/lstm_cell_17/kernel&variables/0/.ATTRIBUTES/VARIABLE_VALUE
`^
VARIABLE_VALUE$lstm_6/lstm_cell_17/recurrent_kernel&variables/1/.ATTRIBUTES/VARIABLE_VALUE
TR
VARIABLE_VALUElstm_6/lstm_cell_17/bias&variables/2/.ATTRIBUTES/VARIABLE_VALUE
VT
VARIABLE_VALUElstm_7/lstm_cell_18/kernel&variables/3/.ATTRIBUTES/VARIABLE_VALUE
`^
VARIABLE_VALUE$lstm_7/lstm_cell_18/recurrent_kernel&variables/4/.ATTRIBUTES/VARIABLE_VALUE
TR
VARIABLE_VALUElstm_7/lstm_cell_18/bias&variables/5/.ATTRIBUTES/VARIABLE_VALUE

E0
 
 

0
1
2
 

!0
"1
#2

!0
"1
#2
 
?
Fmetrics
Glayer_metrics
Hlayer_regularization_losses
,	variables
-trainable_variables
.regularization_losses

Ilayers
Jnon_trainable_variables
 
 
 
 


0
 

$0
%1
&2

$0
%1
&2
 
?
Kmetrics
Llayer_metrics
Mlayer_regularization_losses
6	variables
7trainable_variables
8regularization_losses

Nlayers
Onon_trainable_variables
 
 
 
 

0
 
 
 
 
 
 
4
	Ptotal
	Qcount
R	variables
S	keras_api
 
 
 
 
 
 
 
 
 
 
OM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE
OM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE

P0
Q1

R	variables
??
VARIABLE_VALUERMSprop/dense_18/kernel/rmsTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?
VARIABLE_VALUERMSprop/dense_18/bias/rmsRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?~
VARIABLE_VALUE&RMSprop/lstm_6/lstm_cell_17/kernel/rmsDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE0RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rmsDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUE$RMSprop/lstm_6/lstm_cell_17/bias/rmsDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?~
VARIABLE_VALUE&RMSprop/lstm_7/lstm_cell_18/kernel/rmsDvariables/3/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
??
VARIABLE_VALUE0RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rmsDvariables/4/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
~|
VARIABLE_VALUE$RMSprop/lstm_7/lstm_cell_18/bias/rmsDvariables/5/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUE
?
serving_default_lstm_6_inputPlaceholder*+
_output_shapes
:?????????x*
dtype0* 
shape:?????????x
?
StatefulPartitionedCallStatefulPartitionedCallserving_default_lstm_6_inputlstm_6/lstm_cell_17/kernel$lstm_6/lstm_cell_17/recurrent_kernellstm_6/lstm_cell_17/biaslstm_7/lstm_cell_18/kernel$lstm_7/lstm_cell_18/recurrent_kernellstm_7/lstm_cell_18/biasdense_18/kerneldense_18/bias*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *-
f(R&
$__inference_signature_wrapper_628162
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
?

StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename#dense_18/kernel/Read/ReadVariableOp!dense_18/bias/Read/ReadVariableOp RMSprop/iter/Read/ReadVariableOp!RMSprop/decay/Read/ReadVariableOp)RMSprop/learning_rate/Read/ReadVariableOp$RMSprop/momentum/Read/ReadVariableOpRMSprop/rho/Read/ReadVariableOp.lstm_6/lstm_cell_17/kernel/Read/ReadVariableOp8lstm_6/lstm_cell_17/recurrent_kernel/Read/ReadVariableOp,lstm_6/lstm_cell_17/bias/Read/ReadVariableOp.lstm_7/lstm_cell_18/kernel/Read/ReadVariableOp8lstm_7/lstm_cell_18/recurrent_kernel/Read/ReadVariableOp,lstm_7/lstm_cell_18/bias/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp/RMSprop/dense_18/kernel/rms/Read/ReadVariableOp-RMSprop/dense_18/bias/rms/Read/ReadVariableOp:RMSprop/lstm_6/lstm_cell_17/kernel/rms/Read/ReadVariableOpDRMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms/Read/ReadVariableOp8RMSprop/lstm_6/lstm_cell_17/bias/rms/Read/ReadVariableOp:RMSprop/lstm_7/lstm_cell_18/kernel/rms/Read/ReadVariableOpDRMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms/Read/ReadVariableOp8RMSprop/lstm_7/lstm_cell_18/bias/rms/Read/ReadVariableOpConst*$
Tin
2	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *(
f#R!
__inference__traced_save_630443
?
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenamedense_18/kerneldense_18/biasRMSprop/iterRMSprop/decayRMSprop/learning_rateRMSprop/momentumRMSprop/rholstm_6/lstm_cell_17/kernel$lstm_6/lstm_cell_17/recurrent_kernellstm_6/lstm_cell_17/biaslstm_7/lstm_cell_18/kernel$lstm_7/lstm_cell_18/recurrent_kernellstm_7/lstm_cell_18/biastotalcountRMSprop/dense_18/kernel/rmsRMSprop/dense_18/bias/rms&RMSprop/lstm_6/lstm_cell_17/kernel/rms0RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms$RMSprop/lstm_6/lstm_cell_17/bias/rms&RMSprop/lstm_7/lstm_cell_18/kernel/rms0RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms$RMSprop/lstm_7/lstm_cell_18/bias/rms*#
Tin
2*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *-
config_proto

CPU

GPU 2J 8? *+
f&R$
"__inference__traced_restore_630522??"
??
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628470

inputs6
2lstm_6_lstm_cell_17_matmul_readvariableop_resource8
4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource7
3lstm_6_lstm_cell_17_biasadd_readvariableop_resource6
2lstm_7_lstm_cell_18_matmul_readvariableop_resource8
4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource7
3lstm_7_lstm_cell_18_biasadd_readvariableop_resource+
'dense_18_matmul_readvariableop_resource,
(dense_18_biasadd_readvariableop_resource
identity??dense_18/BiasAdd/ReadVariableOp?dense_18/MatMul/ReadVariableOp?*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?)lstm_6/lstm_cell_17/MatMul/ReadVariableOp?+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?lstm_6/while?*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?)lstm_7/lstm_cell_18/MatMul/ReadVariableOp?+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?lstm_7/whileR
lstm_6/ShapeShapeinputs*
T0*
_output_shapes
:2
lstm_6/Shape?
lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice/stack?
lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_1?
lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_2?
lstm_6/strided_sliceStridedSlicelstm_6/Shape:output:0#lstm_6/strided_slice/stack:output:0%lstm_6/strided_slice/stack_1:output:0%lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slicej
lstm_6/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros/mul/y?
lstm_6/zeros/mulMullstm_6/strided_slice:output:0lstm_6/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros/mulm
lstm_6/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_6/zeros/Less/y?
lstm_6/zeros/LessLesslstm_6/zeros/mul:z:0lstm_6/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros/Lessp
lstm_6/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros/packed/1?
lstm_6/zeros/packedPacklstm_6/strided_slice:output:0lstm_6/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_6/zeros/packedm
lstm_6/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/zeros/Const?
lstm_6/zerosFilllstm_6/zeros/packed:output:0lstm_6/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/zerosn
lstm_6/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros_1/mul/y?
lstm_6/zeros_1/mulMullstm_6/strided_slice:output:0lstm_6/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros_1/mulq
lstm_6/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_6/zeros_1/Less/y?
lstm_6/zeros_1/LessLesslstm_6/zeros_1/mul:z:0lstm_6/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros_1/Lesst
lstm_6/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros_1/packed/1?
lstm_6/zeros_1/packedPacklstm_6/strided_slice:output:0 lstm_6/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_6/zeros_1/packedq
lstm_6/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/zeros_1/Const?
lstm_6/zeros_1Filllstm_6/zeros_1/packed:output:0lstm_6/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/zeros_1?
lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose/perm?
lstm_6/transpose	Transposeinputslstm_6/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2
lstm_6/transposed
lstm_6/Shape_1Shapelstm_6/transpose:y:0*
T0*
_output_shapes
:2
lstm_6/Shape_1?
lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_1/stack?
lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_1?
lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_2?
lstm_6/strided_slice_1StridedSlicelstm_6/Shape_1:output:0%lstm_6/strided_slice_1/stack:output:0'lstm_6/strided_slice_1/stack_1:output:0'lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slice_1?
"lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_6/TensorArrayV2/element_shape?
lstm_6/TensorArrayV2TensorListReserve+lstm_6/TensorArrayV2/element_shape:output:0lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2?
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2>
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_6/transpose:y:0Elstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_6/TensorArrayUnstack/TensorListFromTensor?
lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_2/stack?
lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_1?
lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_2?
lstm_6/strided_slice_2StridedSlicelstm_6/transpose:y:0%lstm_6/strided_slice_2/stack:output:0'lstm_6/strided_slice_2/stack_1:output:0'lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_6/strided_slice_2?
)lstm_6/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp2lstm_6_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02+
)lstm_6/lstm_cell_17/MatMul/ReadVariableOp?
lstm_6/lstm_cell_17/MatMulMatMullstm_6/strided_slice_2:output:01lstm_6/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/MatMul?
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02-
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_6/lstm_cell_17/MatMul_1MatMullstm_6/zeros:output:03lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/MatMul_1?
lstm_6/lstm_cell_17/addAddV2$lstm_6/lstm_cell_17/MatMul:product:0&lstm_6/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/add?
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02,
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_6/lstm_cell_17/BiasAddBiasAddlstm_6/lstm_cell_17/add:z:02lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/BiasAddx
lstm_6/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/lstm_cell_17/Const?
#lstm_6/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2%
#lstm_6/lstm_cell_17/split/split_dim?
lstm_6/lstm_cell_17/splitSplit,lstm_6/lstm_cell_17/split/split_dim:output:0$lstm_6/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_6/lstm_cell_17/split?
lstm_6/lstm_cell_17/SigmoidSigmoid"lstm_6/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid?
lstm_6/lstm_cell_17/Sigmoid_1Sigmoid"lstm_6/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid_1?
lstm_6/lstm_cell_17/mulMul!lstm_6/lstm_cell_17/Sigmoid_1:y:0lstm_6/zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul?
lstm_6/lstm_cell_17/ReluRelu"lstm_6/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Relu?
lstm_6/lstm_cell_17/mul_1Mullstm_6/lstm_cell_17/Sigmoid:y:0&lstm_6/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul_1?
lstm_6/lstm_cell_17/add_1AddV2lstm_6/lstm_cell_17/mul:z:0lstm_6/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/add_1?
lstm_6/lstm_cell_17/Sigmoid_2Sigmoid"lstm_6/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid_2?
lstm_6/lstm_cell_17/Relu_1Relulstm_6/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Relu_1?
lstm_6/lstm_cell_17/mul_2Mul!lstm_6/lstm_cell_17/Sigmoid_2:y:0(lstm_6/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul_2?
$lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2&
$lstm_6/TensorArrayV2_1/element_shape?
lstm_6/TensorArrayV2_1TensorListReserve-lstm_6/TensorArrayV2_1/element_shape:output:0lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2_1\
lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/time?
lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_6/while/maximum_iterationsx
lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/while/loop_counter?
lstm_6/whileWhile"lstm_6/while/loop_counter:output:0(lstm_6/while/maximum_iterations:output:0lstm_6/time:output:0lstm_6/TensorArrayV2_1:handle:0lstm_6/zeros:output:0lstm_6/zeros_1:output:0lstm_6/strided_slice_1:output:0>lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:02lstm_6_lstm_cell_17_matmul_readvariableop_resource4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource3lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*$
bodyR
lstm_6_while_body_628230*$
condR
lstm_6_while_cond_628229*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
lstm_6/while?
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_6/TensorArrayV2Stack/TensorListStackTensorListStacklstm_6/while:output:3@lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02+
)lstm_6/TensorArrayV2Stack/TensorListStack?
lstm_6/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_6/strided_slice_3/stack?
lstm_6/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_6/strided_slice_3/stack_1?
lstm_6/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_3/stack_2?
lstm_6/strided_slice_3StridedSlice2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_6/strided_slice_3/stack:output:0'lstm_6/strided_slice_3/stack_1:output:0'lstm_6/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_6/strided_slice_3?
lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose_1/perm?
lstm_6/transpose_1	Transpose2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_6/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
lstm_6/transpose_1t
lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/runtimeb
lstm_7/ShapeShapelstm_6/transpose_1:y:0*
T0*
_output_shapes
:2
lstm_7/Shape?
lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice/stack?
lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_1?
lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_2?
lstm_7/strided_sliceStridedSlicelstm_7/Shape:output:0#lstm_7/strided_slice/stack:output:0%lstm_7/strided_slice/stack_1:output:0%lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slicej
lstm_7/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros/mul/y?
lstm_7/zeros/mulMullstm_7/strided_slice:output:0lstm_7/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros/mulm
lstm_7/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_7/zeros/Less/y?
lstm_7/zeros/LessLesslstm_7/zeros/mul:z:0lstm_7/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros/Lessp
lstm_7/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros/packed/1?
lstm_7/zeros/packedPacklstm_7/strided_slice:output:0lstm_7/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_7/zeros/packedm
lstm_7/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/zeros/Const?
lstm_7/zerosFilllstm_7/zeros/packed:output:0lstm_7/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/zerosn
lstm_7/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros_1/mul/y?
lstm_7/zeros_1/mulMullstm_7/strided_slice:output:0lstm_7/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros_1/mulq
lstm_7/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_7/zeros_1/Less/y?
lstm_7/zeros_1/LessLesslstm_7/zeros_1/mul:z:0lstm_7/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros_1/Lesst
lstm_7/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros_1/packed/1?
lstm_7/zeros_1/packedPacklstm_7/strided_slice:output:0 lstm_7/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_7/zeros_1/packedq
lstm_7/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/zeros_1/Const?
lstm_7/zeros_1Filllstm_7/zeros_1/packed:output:0lstm_7/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/zeros_1?
lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose/perm?
lstm_7/transpose	Transposelstm_6/transpose_1:y:0lstm_7/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2
lstm_7/transposed
lstm_7/Shape_1Shapelstm_7/transpose:y:0*
T0*
_output_shapes
:2
lstm_7/Shape_1?
lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_1/stack?
lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_1?
lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_2?
lstm_7/strided_slice_1StridedSlicelstm_7/Shape_1:output:0%lstm_7/strided_slice_1/stack:output:0'lstm_7/strided_slice_1/stack_1:output:0'lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slice_1?
"lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_7/TensorArrayV2/element_shape?
lstm_7/TensorArrayV2TensorListReserve+lstm_7/TensorArrayV2/element_shape:output:0lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2?
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2>
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_7/transpose:y:0Elstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_7/TensorArrayUnstack/TensorListFromTensor?
lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_2/stack?
lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_1?
lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_2?
lstm_7/strided_slice_2StridedSlicelstm_7/transpose:y:0%lstm_7/strided_slice_2/stack:output:0'lstm_7/strided_slice_2/stack_1:output:0'lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_7/strided_slice_2?
)lstm_7/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp2lstm_7_lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02+
)lstm_7/lstm_cell_18/MatMul/ReadVariableOp?
lstm_7/lstm_cell_18/MatMulMatMullstm_7/strided_slice_2:output:01lstm_7/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/MatMul?
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02-
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_7/lstm_cell_18/MatMul_1MatMullstm_7/zeros:output:03lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/MatMul_1?
lstm_7/lstm_cell_18/addAddV2$lstm_7/lstm_cell_18/MatMul:product:0&lstm_7/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/add?
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02,
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_7/lstm_cell_18/BiasAddBiasAddlstm_7/lstm_cell_18/add:z:02lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/BiasAddx
lstm_7/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/lstm_cell_18/Const?
#lstm_7/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2%
#lstm_7/lstm_cell_18/split/split_dim?
lstm_7/lstm_cell_18/splitSplit,lstm_7/lstm_cell_18/split/split_dim:output:0$lstm_7/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_7/lstm_cell_18/split?
lstm_7/lstm_cell_18/SigmoidSigmoid"lstm_7/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid?
lstm_7/lstm_cell_18/Sigmoid_1Sigmoid"lstm_7/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid_1?
lstm_7/lstm_cell_18/mulMul!lstm_7/lstm_cell_18/Sigmoid_1:y:0lstm_7/zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul?
lstm_7/lstm_cell_18/ReluRelu"lstm_7/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Relu?
lstm_7/lstm_cell_18/mul_1Mullstm_7/lstm_cell_18/Sigmoid:y:0&lstm_7/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul_1?
lstm_7/lstm_cell_18/add_1AddV2lstm_7/lstm_cell_18/mul:z:0lstm_7/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/add_1?
lstm_7/lstm_cell_18/Sigmoid_2Sigmoid"lstm_7/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid_2?
lstm_7/lstm_cell_18/Relu_1Relulstm_7/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Relu_1?
lstm_7/lstm_cell_18/mul_2Mul!lstm_7/lstm_cell_18/Sigmoid_2:y:0(lstm_7/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul_2?
$lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2&
$lstm_7/TensorArrayV2_1/element_shape?
lstm_7/TensorArrayV2_1TensorListReserve-lstm_7/TensorArrayV2_1/element_shape:output:0lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2_1\
lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/time?
lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_7/while/maximum_iterationsx
lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/while/loop_counter?
lstm_7/whileWhile"lstm_7/while/loop_counter:output:0(lstm_7/while/maximum_iterations:output:0lstm_7/time:output:0lstm_7/TensorArrayV2_1:handle:0lstm_7/zeros:output:0lstm_7/zeros_1:output:0lstm_7/strided_slice_1:output:0>lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:02lstm_7_lstm_cell_18_matmul_readvariableop_resource4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource3lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*$
bodyR
lstm_7_while_body_628379*$
condR
lstm_7_while_cond_628378*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
lstm_7/while?
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_7/TensorArrayV2Stack/TensorListStackTensorListStacklstm_7/while:output:3@lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02+
)lstm_7/TensorArrayV2Stack/TensorListStack?
lstm_7/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_7/strided_slice_3/stack?
lstm_7/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_7/strided_slice_3/stack_1?
lstm_7/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_3/stack_2?
lstm_7/strided_slice_3StridedSlice2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_7/strided_slice_3/stack:output:0'lstm_7/strided_slice_3/stack_1:output:0'lstm_7/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_7/strided_slice_3?
lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose_1/perm?
lstm_7/transpose_1	Transpose2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_7/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
lstm_7/transpose_1t
lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/runtime?
dense_18/MatMul/ReadVariableOpReadVariableOp'dense_18_matmul_readvariableop_resource*
_output_shapes

:*
dtype02 
dense_18/MatMul/ReadVariableOp?
dense_18/MatMulMatMullstm_7/strided_slice_3:output:0&dense_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_18/MatMul?
dense_18/BiasAdd/ReadVariableOpReadVariableOp(dense_18_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_18/BiasAdd/ReadVariableOp?
dense_18/BiasAddBiasAdddense_18/MatMul:product:0'dense_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_18/BiasAdd?
IdentityIdentitydense_18/BiasAdd:output:0 ^dense_18/BiasAdd/ReadVariableOp^dense_18/MatMul/ReadVariableOp+^lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp*^lstm_6/lstm_cell_17/MatMul/ReadVariableOp,^lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp^lstm_6/while+^lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp*^lstm_7/lstm_cell_18/MatMul/ReadVariableOp,^lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp^lstm_7/while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2B
dense_18/BiasAdd/ReadVariableOpdense_18/BiasAdd/ReadVariableOp2@
dense_18/MatMul/ReadVariableOpdense_18/MatMul/ReadVariableOp2X
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp2V
)lstm_6/lstm_cell_17/MatMul/ReadVariableOp)lstm_6/lstm_cell_17/MatMul/ReadVariableOp2Z
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp2
lstm_6/whilelstm_6/while2X
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp2V
)lstm_7/lstm_cell_18/MatMul/ReadVariableOp)lstm_7/lstm_cell_18/MatMul/ReadVariableOp2Z
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp2
lstm_7/whilelstm_7/while:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?B
?
while_body_629369
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?D
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_626699

inputs
lstm_cell_17_626617
lstm_cell_17_626619
lstm_cell_17_626621
identity??$lstm_cell_17/StatefulPartitionedCall?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
$lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0zeros_1:output:0lstm_cell_17_626617lstm_cell_17_626619lstm_cell_17_626621*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6262042&
$lstm_cell_17/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_17_626617lstm_cell_17_626619lstm_cell_17_626621*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_626630*
condR
while_cond_626629*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0%^lstm_cell_17/StatefulPartitionedCall^while*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2L
$lstm_cell_17/StatefulPartitionedCall$lstm_cell_17/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :??????????????????
 
_user_specified_nameinputs
?
?
'__inference_lstm_7_layer_call_fn_629793
inputs_0
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6271772
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?%
?
while_body_627240
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_lstm_cell_18_627264_0
while_lstm_cell_18_627266_0
while_lstm_cell_18_627268_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_lstm_cell_18_627264
while_lstm_cell_18_627266
while_lstm_cell_18_627268??*while/lstm_cell_18/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
*while/lstm_cell_18/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_18_627264_0while_lstm_cell_18_627266_0while_lstm_cell_18_627268_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6268142,
*while/lstm_cell_18/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_18/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity3while/lstm_cell_18/StatefulPartitionedCall:output:1+^while/lstm_cell_18/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identity3while/lstm_cell_18/StatefulPartitionedCall:output:2+^while/lstm_cell_18/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"8
while_lstm_cell_18_627264while_lstm_cell_18_627264_0"8
while_lstm_cell_18_627266while_lstm_cell_18_627266_0"8
while_lstm_cell_18_627268while_lstm_cell_18_627268_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2X
*while/lstm_cell_18/StatefulPartitionedCall*while/lstm_cell_18/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
.__inference_sequential_12_layer_call_fn_628820

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
	unknown_5
	unknown_6
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *R
fMRK
I__inference_sequential_12_layer_call_and_return_conditional_losses_6281122
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?b
?
&sequential_12_lstm_7_while_body_626007F
Bsequential_12_lstm_7_while_sequential_12_lstm_7_while_loop_counterL
Hsequential_12_lstm_7_while_sequential_12_lstm_7_while_maximum_iterations*
&sequential_12_lstm_7_while_placeholder,
(sequential_12_lstm_7_while_placeholder_1,
(sequential_12_lstm_7_while_placeholder_2,
(sequential_12_lstm_7_while_placeholder_3E
Asequential_12_lstm_7_while_sequential_12_lstm_7_strided_slice_1_0?
}sequential_12_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_7_tensorarrayunstack_tensorlistfromtensor_0L
Hsequential_12_lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0N
Jsequential_12_lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0M
Isequential_12_lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0'
#sequential_12_lstm_7_while_identity)
%sequential_12_lstm_7_while_identity_1)
%sequential_12_lstm_7_while_identity_2)
%sequential_12_lstm_7_while_identity_3)
%sequential_12_lstm_7_while_identity_4)
%sequential_12_lstm_7_while_identity_5C
?sequential_12_lstm_7_while_sequential_12_lstm_7_strided_slice_1
{sequential_12_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_7_tensorarrayunstack_tensorlistfromtensorJ
Fsequential_12_lstm_7_while_lstm_cell_18_matmul_readvariableop_resourceL
Hsequential_12_lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resourceK
Gsequential_12_lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource??>sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?=sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp??sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
Lsequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2N
Lsequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
>sequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem}sequential_12_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_7_tensorarrayunstack_tensorlistfromtensor_0&sequential_12_lstm_7_while_placeholderUsequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02@
>sequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItem?
=sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOpHsequential_12_lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02?
=sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp?
.sequential_12/lstm_7/while/lstm_cell_18/MatMulMatMulEsequential_12/lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:0Esequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<20
.sequential_12/lstm_7/while/lstm_cell_18/MatMul?
?sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOpJsequential_12_lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02A
?sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
0sequential_12/lstm_7/while/lstm_cell_18/MatMul_1MatMul(sequential_12_lstm_7_while_placeholder_2Gsequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<22
0sequential_12/lstm_7/while/lstm_cell_18/MatMul_1?
+sequential_12/lstm_7/while/lstm_cell_18/addAddV28sequential_12/lstm_7/while/lstm_cell_18/MatMul:product:0:sequential_12/lstm_7/while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2-
+sequential_12/lstm_7/while/lstm_cell_18/add?
>sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOpIsequential_12_lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02@
>sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?
/sequential_12/lstm_7/while/lstm_cell_18/BiasAddBiasAdd/sequential_12/lstm_7/while/lstm_cell_18/add:z:0Fsequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<21
/sequential_12/lstm_7/while/lstm_cell_18/BiasAdd?
-sequential_12/lstm_7/while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2/
-sequential_12/lstm_7/while/lstm_cell_18/Const?
7sequential_12/lstm_7/while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :29
7sequential_12/lstm_7/while/lstm_cell_18/split/split_dim?
-sequential_12/lstm_7/while/lstm_cell_18/splitSplit@sequential_12/lstm_7/while/lstm_cell_18/split/split_dim:output:08sequential_12/lstm_7/while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2/
-sequential_12/lstm_7/while/lstm_cell_18/split?
/sequential_12/lstm_7/while/lstm_cell_18/SigmoidSigmoid6sequential_12/lstm_7/while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????21
/sequential_12/lstm_7/while/lstm_cell_18/Sigmoid?
1sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_1Sigmoid6sequential_12/lstm_7/while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????23
1sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_1?
+sequential_12/lstm_7/while/lstm_cell_18/mulMul5sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_1:y:0(sequential_12_lstm_7_while_placeholder_3*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_7/while/lstm_cell_18/mul?
,sequential_12/lstm_7/while/lstm_cell_18/ReluRelu6sequential_12/lstm_7/while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2.
,sequential_12/lstm_7/while/lstm_cell_18/Relu?
-sequential_12/lstm_7/while/lstm_cell_18/mul_1Mul3sequential_12/lstm_7/while/lstm_cell_18/Sigmoid:y:0:sequential_12/lstm_7/while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_7/while/lstm_cell_18/mul_1?
-sequential_12/lstm_7/while/lstm_cell_18/add_1AddV2/sequential_12/lstm_7/while/lstm_cell_18/mul:z:01sequential_12/lstm_7/while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_7/while/lstm_cell_18/add_1?
1sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_2Sigmoid6sequential_12/lstm_7/while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????23
1sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_2?
.sequential_12/lstm_7/while/lstm_cell_18/Relu_1Relu1sequential_12/lstm_7/while/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????20
.sequential_12/lstm_7/while/lstm_cell_18/Relu_1?
-sequential_12/lstm_7/while/lstm_cell_18/mul_2Mul5sequential_12/lstm_7/while/lstm_cell_18/Sigmoid_2:y:0<sequential_12/lstm_7/while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_7/while/lstm_cell_18/mul_2?
?sequential_12/lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem(sequential_12_lstm_7_while_placeholder_1&sequential_12_lstm_7_while_placeholder1sequential_12/lstm_7/while/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02A
?sequential_12/lstm_7/while/TensorArrayV2Write/TensorListSetItem?
 sequential_12/lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_12/lstm_7/while/add/y?
sequential_12/lstm_7/while/addAddV2&sequential_12_lstm_7_while_placeholder)sequential_12/lstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2 
sequential_12/lstm_7/while/add?
"sequential_12/lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_12/lstm_7/while/add_1/y?
 sequential_12/lstm_7/while/add_1AddV2Bsequential_12_lstm_7_while_sequential_12_lstm_7_while_loop_counter+sequential_12/lstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2"
 sequential_12/lstm_7/while/add_1?
#sequential_12/lstm_7/while/IdentityIdentity$sequential_12/lstm_7/while/add_1:z:0?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2%
#sequential_12/lstm_7/while/Identity?
%sequential_12/lstm_7/while/Identity_1IdentityHsequential_12_lstm_7_while_sequential_12_lstm_7_while_maximum_iterations?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_7/while/Identity_1?
%sequential_12/lstm_7/while/Identity_2Identity"sequential_12/lstm_7/while/add:z:0?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_7/while/Identity_2?
%sequential_12/lstm_7/while/Identity_3IdentityOsequential_12/lstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:0?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_7/while/Identity_3?
%sequential_12/lstm_7/while/Identity_4Identity1sequential_12/lstm_7/while/lstm_cell_18/mul_2:z:0?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_7/while/Identity_4?
%sequential_12/lstm_7/while/Identity_5Identity1sequential_12/lstm_7/while/lstm_cell_18/add_1:z:0?^sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>^sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp@^sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_7/while/Identity_5"S
#sequential_12_lstm_7_while_identity,sequential_12/lstm_7/while/Identity:output:0"W
%sequential_12_lstm_7_while_identity_1.sequential_12/lstm_7/while/Identity_1:output:0"W
%sequential_12_lstm_7_while_identity_2.sequential_12/lstm_7/while/Identity_2:output:0"W
%sequential_12_lstm_7_while_identity_3.sequential_12/lstm_7/while/Identity_3:output:0"W
%sequential_12_lstm_7_while_identity_4.sequential_12/lstm_7/while/Identity_4:output:0"W
%sequential_12_lstm_7_while_identity_5.sequential_12/lstm_7/while/Identity_5:output:0"?
Gsequential_12_lstm_7_while_lstm_cell_18_biasadd_readvariableop_resourceIsequential_12_lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0"?
Hsequential_12_lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resourceJsequential_12_lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0"?
Fsequential_12_lstm_7_while_lstm_cell_18_matmul_readvariableop_resourceHsequential_12_lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0"?
?sequential_12_lstm_7_while_sequential_12_lstm_7_strided_slice_1Asequential_12_lstm_7_while_sequential_12_lstm_7_strided_slice_1_0"?
{sequential_12_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_7_tensorarrayunstack_tensorlistfromtensor}sequential_12_lstm_7_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2?
>sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp>sequential_12/lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp2~
=sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp=sequential_12/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2?
?sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?sequential_12/lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?B
?
while_body_629544
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
??
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628778

inputs6
2lstm_6_lstm_cell_17_matmul_readvariableop_resource8
4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource7
3lstm_6_lstm_cell_17_biasadd_readvariableop_resource6
2lstm_7_lstm_cell_18_matmul_readvariableop_resource8
4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource7
3lstm_7_lstm_cell_18_biasadd_readvariableop_resource+
'dense_18_matmul_readvariableop_resource,
(dense_18_biasadd_readvariableop_resource
identity??dense_18/BiasAdd/ReadVariableOp?dense_18/MatMul/ReadVariableOp?*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?)lstm_6/lstm_cell_17/MatMul/ReadVariableOp?+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?lstm_6/while?*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?)lstm_7/lstm_cell_18/MatMul/ReadVariableOp?+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?lstm_7/whileR
lstm_6/ShapeShapeinputs*
T0*
_output_shapes
:2
lstm_6/Shape?
lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice/stack?
lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_1?
lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_6/strided_slice/stack_2?
lstm_6/strided_sliceStridedSlicelstm_6/Shape:output:0#lstm_6/strided_slice/stack:output:0%lstm_6/strided_slice/stack_1:output:0%lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slicej
lstm_6/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros/mul/y?
lstm_6/zeros/mulMullstm_6/strided_slice:output:0lstm_6/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros/mulm
lstm_6/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_6/zeros/Less/y?
lstm_6/zeros/LessLesslstm_6/zeros/mul:z:0lstm_6/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros/Lessp
lstm_6/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros/packed/1?
lstm_6/zeros/packedPacklstm_6/strided_slice:output:0lstm_6/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_6/zeros/packedm
lstm_6/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/zeros/Const?
lstm_6/zerosFilllstm_6/zeros/packed:output:0lstm_6/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/zerosn
lstm_6/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros_1/mul/y?
lstm_6/zeros_1/mulMullstm_6/strided_slice:output:0lstm_6/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros_1/mulq
lstm_6/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_6/zeros_1/Less/y?
lstm_6/zeros_1/LessLesslstm_6/zeros_1/mul:z:0lstm_6/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_6/zeros_1/Lesst
lstm_6/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/zeros_1/packed/1?
lstm_6/zeros_1/packedPacklstm_6/strided_slice:output:0 lstm_6/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_6/zeros_1/packedq
lstm_6/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/zeros_1/Const?
lstm_6/zeros_1Filllstm_6/zeros_1/packed:output:0lstm_6/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/zeros_1?
lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose/perm?
lstm_6/transpose	Transposeinputslstm_6/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2
lstm_6/transposed
lstm_6/Shape_1Shapelstm_6/transpose:y:0*
T0*
_output_shapes
:2
lstm_6/Shape_1?
lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_1/stack?
lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_1?
lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_1/stack_2?
lstm_6/strided_slice_1StridedSlicelstm_6/Shape_1:output:0%lstm_6/strided_slice_1/stack:output:0'lstm_6/strided_slice_1/stack_1:output:0'lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_6/strided_slice_1?
"lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_6/TensorArrayV2/element_shape?
lstm_6/TensorArrayV2TensorListReserve+lstm_6/TensorArrayV2/element_shape:output:0lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2?
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2>
<lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_6/transpose:y:0Elstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_6/TensorArrayUnstack/TensorListFromTensor?
lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_6/strided_slice_2/stack?
lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_1?
lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_2/stack_2?
lstm_6/strided_slice_2StridedSlicelstm_6/transpose:y:0%lstm_6/strided_slice_2/stack:output:0'lstm_6/strided_slice_2/stack_1:output:0'lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_6/strided_slice_2?
)lstm_6/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp2lstm_6_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02+
)lstm_6/lstm_cell_17/MatMul/ReadVariableOp?
lstm_6/lstm_cell_17/MatMulMatMullstm_6/strided_slice_2:output:01lstm_6/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/MatMul?
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02-
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_6/lstm_cell_17/MatMul_1MatMullstm_6/zeros:output:03lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/MatMul_1?
lstm_6/lstm_cell_17/addAddV2$lstm_6/lstm_cell_17/MatMul:product:0&lstm_6/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/add?
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp3lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02,
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_6/lstm_cell_17/BiasAddBiasAddlstm_6/lstm_cell_17/add:z:02lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_6/lstm_cell_17/BiasAddx
lstm_6/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/lstm_cell_17/Const?
#lstm_6/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2%
#lstm_6/lstm_cell_17/split/split_dim?
lstm_6/lstm_cell_17/splitSplit,lstm_6/lstm_cell_17/split/split_dim:output:0$lstm_6/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_6/lstm_cell_17/split?
lstm_6/lstm_cell_17/SigmoidSigmoid"lstm_6/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid?
lstm_6/lstm_cell_17/Sigmoid_1Sigmoid"lstm_6/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid_1?
lstm_6/lstm_cell_17/mulMul!lstm_6/lstm_cell_17/Sigmoid_1:y:0lstm_6/zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul?
lstm_6/lstm_cell_17/ReluRelu"lstm_6/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Relu?
lstm_6/lstm_cell_17/mul_1Mullstm_6/lstm_cell_17/Sigmoid:y:0&lstm_6/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul_1?
lstm_6/lstm_cell_17/add_1AddV2lstm_6/lstm_cell_17/mul:z:0lstm_6/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/add_1?
lstm_6/lstm_cell_17/Sigmoid_2Sigmoid"lstm_6/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Sigmoid_2?
lstm_6/lstm_cell_17/Relu_1Relulstm_6/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/Relu_1?
lstm_6/lstm_cell_17/mul_2Mul!lstm_6/lstm_cell_17/Sigmoid_2:y:0(lstm_6/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_6/lstm_cell_17/mul_2?
$lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2&
$lstm_6/TensorArrayV2_1/element_shape?
lstm_6/TensorArrayV2_1TensorListReserve-lstm_6/TensorArrayV2_1/element_shape:output:0lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_6/TensorArrayV2_1\
lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/time?
lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_6/while/maximum_iterationsx
lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_6/while/loop_counter?
lstm_6/whileWhile"lstm_6/while/loop_counter:output:0(lstm_6/while/maximum_iterations:output:0lstm_6/time:output:0lstm_6/TensorArrayV2_1:handle:0lstm_6/zeros:output:0lstm_6/zeros_1:output:0lstm_6/strided_slice_1:output:0>lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:02lstm_6_lstm_cell_17_matmul_readvariableop_resource4lstm_6_lstm_cell_17_matmul_1_readvariableop_resource3lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*$
bodyR
lstm_6_while_body_628538*$
condR
lstm_6_while_cond_628537*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
lstm_6/while?
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_6/TensorArrayV2Stack/TensorListStackTensorListStacklstm_6/while:output:3@lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02+
)lstm_6/TensorArrayV2Stack/TensorListStack?
lstm_6/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_6/strided_slice_3/stack?
lstm_6/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_6/strided_slice_3/stack_1?
lstm_6/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_6/strided_slice_3/stack_2?
lstm_6/strided_slice_3StridedSlice2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_6/strided_slice_3/stack:output:0'lstm_6/strided_slice_3/stack_1:output:0'lstm_6/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_6/strided_slice_3?
lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_6/transpose_1/perm?
lstm_6/transpose_1	Transpose2lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_6/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
lstm_6/transpose_1t
lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_6/runtimeb
lstm_7/ShapeShapelstm_6/transpose_1:y:0*
T0*
_output_shapes
:2
lstm_7/Shape?
lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice/stack?
lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_1?
lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
lstm_7/strided_slice/stack_2?
lstm_7/strided_sliceStridedSlicelstm_7/Shape:output:0#lstm_7/strided_slice/stack:output:0%lstm_7/strided_slice/stack_1:output:0%lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slicej
lstm_7/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros/mul/y?
lstm_7/zeros/mulMullstm_7/strided_slice:output:0lstm_7/zeros/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros/mulm
lstm_7/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_7/zeros/Less/y?
lstm_7/zeros/LessLesslstm_7/zeros/mul:z:0lstm_7/zeros/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros/Lessp
lstm_7/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros/packed/1?
lstm_7/zeros/packedPacklstm_7/strided_slice:output:0lstm_7/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_7/zeros/packedm
lstm_7/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/zeros/Const?
lstm_7/zerosFilllstm_7/zeros/packed:output:0lstm_7/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/zerosn
lstm_7/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros_1/mul/y?
lstm_7/zeros_1/mulMullstm_7/strided_slice:output:0lstm_7/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros_1/mulq
lstm_7/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
lstm_7/zeros_1/Less/y?
lstm_7/zeros_1/LessLesslstm_7/zeros_1/mul:z:0lstm_7/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
lstm_7/zeros_1/Lesst
lstm_7/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/zeros_1/packed/1?
lstm_7/zeros_1/packedPacklstm_7/strided_slice:output:0 lstm_7/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
lstm_7/zeros_1/packedq
lstm_7/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/zeros_1/Const?
lstm_7/zeros_1Filllstm_7/zeros_1/packed:output:0lstm_7/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/zeros_1?
lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose/perm?
lstm_7/transpose	Transposelstm_6/transpose_1:y:0lstm_7/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2
lstm_7/transposed
lstm_7/Shape_1Shapelstm_7/transpose:y:0*
T0*
_output_shapes
:2
lstm_7/Shape_1?
lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_1/stack?
lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_1?
lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_1/stack_2?
lstm_7/strided_slice_1StridedSlicelstm_7/Shape_1:output:0%lstm_7/strided_slice_1/stack:output:0'lstm_7/strided_slice_1/stack_1:output:0'lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
lstm_7/strided_slice_1?
"lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2$
"lstm_7/TensorArrayV2/element_shape?
lstm_7/TensorArrayV2TensorListReserve+lstm_7/TensorArrayV2/element_shape:output:0lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2?
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2>
<lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
.lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensorlstm_7/transpose:y:0Elstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type020
.lstm_7/TensorArrayUnstack/TensorListFromTensor?
lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
lstm_7/strided_slice_2/stack?
lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_1?
lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_2/stack_2?
lstm_7/strided_slice_2StridedSlicelstm_7/transpose:y:0%lstm_7/strided_slice_2/stack:output:0'lstm_7/strided_slice_2/stack_1:output:0'lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_7/strided_slice_2?
)lstm_7/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp2lstm_7_lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02+
)lstm_7/lstm_cell_18/MatMul/ReadVariableOp?
lstm_7/lstm_cell_18/MatMulMatMullstm_7/strided_slice_2:output:01lstm_7/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/MatMul?
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02-
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_7/lstm_cell_18/MatMul_1MatMullstm_7/zeros:output:03lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/MatMul_1?
lstm_7/lstm_cell_18/addAddV2$lstm_7/lstm_cell_18/MatMul:product:0&lstm_7/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/add?
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp3lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02,
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_7/lstm_cell_18/BiasAddBiasAddlstm_7/lstm_cell_18/add:z:02lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_7/lstm_cell_18/BiasAddx
lstm_7/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/lstm_cell_18/Const?
#lstm_7/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2%
#lstm_7/lstm_cell_18/split/split_dim?
lstm_7/lstm_cell_18/splitSplit,lstm_7/lstm_cell_18/split/split_dim:output:0$lstm_7/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_7/lstm_cell_18/split?
lstm_7/lstm_cell_18/SigmoidSigmoid"lstm_7/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid?
lstm_7/lstm_cell_18/Sigmoid_1Sigmoid"lstm_7/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid_1?
lstm_7/lstm_cell_18/mulMul!lstm_7/lstm_cell_18/Sigmoid_1:y:0lstm_7/zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul?
lstm_7/lstm_cell_18/ReluRelu"lstm_7/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Relu?
lstm_7/lstm_cell_18/mul_1Mullstm_7/lstm_cell_18/Sigmoid:y:0&lstm_7/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul_1?
lstm_7/lstm_cell_18/add_1AddV2lstm_7/lstm_cell_18/mul:z:0lstm_7/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/add_1?
lstm_7/lstm_cell_18/Sigmoid_2Sigmoid"lstm_7/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Sigmoid_2?
lstm_7/lstm_cell_18/Relu_1Relulstm_7/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/Relu_1?
lstm_7/lstm_cell_18/mul_2Mul!lstm_7/lstm_cell_18/Sigmoid_2:y:0(lstm_7/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_7/lstm_cell_18/mul_2?
$lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2&
$lstm_7/TensorArrayV2_1/element_shape?
lstm_7/TensorArrayV2_1TensorListReserve-lstm_7/TensorArrayV2_1/element_shape:output:0lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
lstm_7/TensorArrayV2_1\
lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/time?
lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2!
lstm_7/while/maximum_iterationsx
lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
lstm_7/while/loop_counter?
lstm_7/whileWhile"lstm_7/while/loop_counter:output:0(lstm_7/while/maximum_iterations:output:0lstm_7/time:output:0lstm_7/TensorArrayV2_1:handle:0lstm_7/zeros:output:0lstm_7/zeros_1:output:0lstm_7/strided_slice_1:output:0>lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:02lstm_7_lstm_cell_18_matmul_readvariableop_resource4lstm_7_lstm_cell_18_matmul_1_readvariableop_resource3lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*$
bodyR
lstm_7_while_body_628687*$
condR
lstm_7_while_cond_628686*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
lstm_7/while?
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
)lstm_7/TensorArrayV2Stack/TensorListStackTensorListStacklstm_7/while:output:3@lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02+
)lstm_7/TensorArrayV2Stack/TensorListStack?
lstm_7/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
lstm_7/strided_slice_3/stack?
lstm_7/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2 
lstm_7/strided_slice_3/stack_1?
lstm_7/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2 
lstm_7/strided_slice_3/stack_2?
lstm_7/strided_slice_3StridedSlice2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0%lstm_7/strided_slice_3/stack:output:0'lstm_7/strided_slice_3/stack_1:output:0'lstm_7/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
lstm_7/strided_slice_3?
lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
lstm_7/transpose_1/perm?
lstm_7/transpose_1	Transpose2lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0 lstm_7/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
lstm_7/transpose_1t
lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
lstm_7/runtime?
dense_18/MatMul/ReadVariableOpReadVariableOp'dense_18_matmul_readvariableop_resource*
_output_shapes

:*
dtype02 
dense_18/MatMul/ReadVariableOp?
dense_18/MatMulMatMullstm_7/strided_slice_3:output:0&dense_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_18/MatMul?
dense_18/BiasAdd/ReadVariableOpReadVariableOp(dense_18_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02!
dense_18/BiasAdd/ReadVariableOp?
dense_18/BiasAddBiasAdddense_18/MatMul:product:0'dense_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
dense_18/BiasAdd?
IdentityIdentitydense_18/BiasAdd:output:0 ^dense_18/BiasAdd/ReadVariableOp^dense_18/MatMul/ReadVariableOp+^lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp*^lstm_6/lstm_cell_17/MatMul/ReadVariableOp,^lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp^lstm_6/while+^lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp*^lstm_7/lstm_cell_18/MatMul/ReadVariableOp,^lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp^lstm_7/while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2B
dense_18/BiasAdd/ReadVariableOpdense_18/BiasAdd/ReadVariableOp2@
dense_18/MatMul/ReadVariableOpdense_18/MatMul/ReadVariableOp2X
*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp*lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp2V
)lstm_6/lstm_cell_17/MatMul/ReadVariableOp)lstm_6/lstm_cell_17/MatMul/ReadVariableOp2Z
+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp+lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp2
lstm_6/whilelstm_6/while2X
*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp*lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp2V
)lstm_7/lstm_cell_18/MatMul/ReadVariableOp)lstm_7/lstm_cell_18/MatMul/ReadVariableOp2Z
+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp+lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp2
lstm_7/whilelstm_7/while:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_630110

inputs/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_630025*
condR
while_cond_630024*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
while_cond_626629
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_626629___redundant_placeholder04
0while_while_cond_626629___redundant_placeholder14
0while_while_cond_626629___redundant_placeholder24
0while_while_cond_626629___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_626781

inputs

states
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_namestates:OK
'
_output_shapes
:?????????
 
_user_specified_namestates
?
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628068

inputs
lstm_6_628048
lstm_6_628050
lstm_6_628052
lstm_7_628055
lstm_7_628057
lstm_7_628059
dense_18_628062
dense_18_628064
identity?? dense_18/StatefulPartitionedCall?lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCallinputslstm_6_628048lstm_6_628050lstm_6_628052*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6274742 
lstm_6/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0lstm_7_628055lstm_7_628057lstm_7_628059*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6278092 
lstm_7/StatefulPartitionedCall?
 dense_18/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0dense_18_628062dense_18_628064*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *M
fHRF
D__inference_dense_18_layer_call_and_return_conditional_losses_6280022"
 dense_18/StatefulPartitionedCall?
IdentityIdentity)dense_18/StatefulPartitionedCall:output:0!^dense_18/StatefulPartitionedCall^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2D
 dense_18/StatefulPartitionedCall dense_18/StatefulPartitionedCall2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
&sequential_12_lstm_7_while_cond_626006F
Bsequential_12_lstm_7_while_sequential_12_lstm_7_while_loop_counterL
Hsequential_12_lstm_7_while_sequential_12_lstm_7_while_maximum_iterations*
&sequential_12_lstm_7_while_placeholder,
(sequential_12_lstm_7_while_placeholder_1,
(sequential_12_lstm_7_while_placeholder_2,
(sequential_12_lstm_7_while_placeholder_3H
Dsequential_12_lstm_7_while_less_sequential_12_lstm_7_strided_slice_1^
Zsequential_12_lstm_7_while_sequential_12_lstm_7_while_cond_626006___redundant_placeholder0^
Zsequential_12_lstm_7_while_sequential_12_lstm_7_while_cond_626006___redundant_placeholder1^
Zsequential_12_lstm_7_while_sequential_12_lstm_7_while_cond_626006___redundant_placeholder2^
Zsequential_12_lstm_7_while_sequential_12_lstm_7_while_cond_626006___redundant_placeholder3'
#sequential_12_lstm_7_while_identity
?
sequential_12/lstm_7/while/LessLess&sequential_12_lstm_7_while_placeholderDsequential_12_lstm_7_while_less_sequential_12_lstm_7_strided_slice_1*
T0*
_output_shapes
: 2!
sequential_12/lstm_7/while/Less?
#sequential_12/lstm_7/while/IdentityIdentity#sequential_12/lstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2%
#sequential_12/lstm_7/while/Identity"S
#sequential_12_lstm_7_while_identity,sequential_12/lstm_7/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
'__inference_lstm_7_layer_call_fn_630121

inputs
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6278092
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
while_cond_627541
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627541___redundant_placeholder04
0while_while_cond_627541___redundant_placeholder14
0while_while_cond_627541___redundant_placeholder24
0while_while_cond_627541___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629301

inputs/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629216*
condR
while_cond_629215*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
while_cond_629696
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629696___redundant_placeholder04
0while_while_cond_629696___redundant_placeholder14
0while_while_cond_629696___redundant_placeholder24
0while_while_cond_629696___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?	
?
D__inference_dense_18_layer_call_and_return_conditional_losses_630142

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdd?
IdentityIdentityBiasAdd:output:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?	
?
lstm_6_while_cond_628537*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3,
(lstm_6_while_less_lstm_6_strided_slice_1B
>lstm_6_while_lstm_6_while_cond_628537___redundant_placeholder0B
>lstm_6_while_lstm_6_while_cond_628537___redundant_placeholder1B
>lstm_6_while_lstm_6_while_cond_628537___redundant_placeholder2B
>lstm_6_while_lstm_6_while_cond_628537___redundant_placeholder3
lstm_6_while_identity
?
lstm_6/while/LessLesslstm_6_while_placeholder(lstm_6_while_less_lstm_6_strided_slice_1*
T0*
_output_shapes
: 2
lstm_6/while/Lessr
lstm_6/while/IdentityIdentitylstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_6/while/Identity"7
lstm_6_while_identitylstm_6/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
'__inference_lstm_6_layer_call_fn_629476

inputs
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6276272
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?%
?
while_body_626630
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_lstm_cell_17_626654_0
while_lstm_cell_17_626656_0
while_lstm_cell_17_626658_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_lstm_cell_17_626654
while_lstm_cell_17_626656
while_lstm_cell_17_626658??*while/lstm_cell_17/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
*while/lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_17_626654_0while_lstm_cell_17_626656_0while_lstm_cell_17_626658_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6262042,
*while/lstm_cell_17/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_17/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity3while/lstm_cell_17/StatefulPartitionedCall:output:1+^while/lstm_cell_17/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identity3while/lstm_cell_17/StatefulPartitionedCall:output:2+^while/lstm_cell_17/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"8
while_lstm_cell_17_626654while_lstm_cell_17_626654_0"8
while_lstm_cell_17_626656while_lstm_cell_17_626656_0"8
while_lstm_cell_17_626658while_lstm_cell_17_626658_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2X
*while/lstm_cell_17/StatefulPartitionedCall*while/lstm_cell_17/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_626171

inputs

states
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_namestates:OK
'
_output_shapes
:?????????
 
_user_specified_namestates
?B
?
while_body_629216
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_629368
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629368___redundant_placeholder04
0while_while_cond_629368___redundant_placeholder14
0while_while_cond_629368___redundant_placeholder24
0while_while_cond_629368___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?8
?
__inference__traced_save_630443
file_prefix.
*savev2_dense_18_kernel_read_readvariableop,
(savev2_dense_18_bias_read_readvariableop+
'savev2_rmsprop_iter_read_readvariableop	,
(savev2_rmsprop_decay_read_readvariableop4
0savev2_rmsprop_learning_rate_read_readvariableop/
+savev2_rmsprop_momentum_read_readvariableop*
&savev2_rmsprop_rho_read_readvariableop9
5savev2_lstm_6_lstm_cell_17_kernel_read_readvariableopC
?savev2_lstm_6_lstm_cell_17_recurrent_kernel_read_readvariableop7
3savev2_lstm_6_lstm_cell_17_bias_read_readvariableop9
5savev2_lstm_7_lstm_cell_18_kernel_read_readvariableopC
?savev2_lstm_7_lstm_cell_18_recurrent_kernel_read_readvariableop7
3savev2_lstm_7_lstm_cell_18_bias_read_readvariableop$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop:
6savev2_rmsprop_dense_18_kernel_rms_read_readvariableop8
4savev2_rmsprop_dense_18_bias_rms_read_readvariableopE
Asavev2_rmsprop_lstm_6_lstm_cell_17_kernel_rms_read_readvariableopO
Ksavev2_rmsprop_lstm_6_lstm_cell_17_recurrent_kernel_rms_read_readvariableopC
?savev2_rmsprop_lstm_6_lstm_cell_17_bias_rms_read_readvariableopE
Asavev2_rmsprop_lstm_7_lstm_cell_18_kernel_rms_read_readvariableopO
Ksavev2_rmsprop_lstm_7_lstm_cell_18_recurrent_kernel_rms_read_readvariableopC
?savev2_rmsprop_lstm_7_lstm_cell_18_bias_rms_read_readvariableop
savev2_const

identity_1??MergeV2Checkpoints?
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*2
StaticRegexFullMatchc
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.part2
Constl
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part2	
Const_1?
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: 2
Selectt

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: 2

StringJoinZ

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :2

num_shards
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 2
ShardedFilename/shard?
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: 2
ShardedFilename?
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?

value?
B?
B6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/3/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/4/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/5/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
SaveV2/tensor_names?
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*C
value:B8B B B B B B B B B B B B B B B B B B B B B B B B 2
SaveV2/shape_and_slices?
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:0*savev2_dense_18_kernel_read_readvariableop(savev2_dense_18_bias_read_readvariableop'savev2_rmsprop_iter_read_readvariableop(savev2_rmsprop_decay_read_readvariableop0savev2_rmsprop_learning_rate_read_readvariableop+savev2_rmsprop_momentum_read_readvariableop&savev2_rmsprop_rho_read_readvariableop5savev2_lstm_6_lstm_cell_17_kernel_read_readvariableop?savev2_lstm_6_lstm_cell_17_recurrent_kernel_read_readvariableop3savev2_lstm_6_lstm_cell_17_bias_read_readvariableop5savev2_lstm_7_lstm_cell_18_kernel_read_readvariableop?savev2_lstm_7_lstm_cell_18_recurrent_kernel_read_readvariableop3savev2_lstm_7_lstm_cell_18_bias_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop6savev2_rmsprop_dense_18_kernel_rms_read_readvariableop4savev2_rmsprop_dense_18_bias_rms_read_readvariableopAsavev2_rmsprop_lstm_6_lstm_cell_17_kernel_rms_read_readvariableopKsavev2_rmsprop_lstm_6_lstm_cell_17_recurrent_kernel_rms_read_readvariableop?savev2_rmsprop_lstm_6_lstm_cell_17_bias_rms_read_readvariableopAsavev2_rmsprop_lstm_7_lstm_cell_18_kernel_rms_read_readvariableopKsavev2_rmsprop_lstm_7_lstm_cell_18_recurrent_kernel_rms_read_readvariableop?savev2_rmsprop_lstm_7_lstm_cell_18_bias_rms_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *&
dtypes
2	2
SaveV2?
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:2(
&MergeV2Checkpoints/checkpoint_prefixes?
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 2
MergeV2Checkpointsr
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: 2

Identitym

Identity_1IdentityIdentity:output:0^MergeV2Checkpoints*
T0*
_output_shapes
: 2

Identity_1"!

identity_1Identity_1:output:0*?
_input_shapes?
?: ::: : : : : :<:<:<:<:<:<: : :::<:<:<:<:<:<: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:$ 

_output_shapes

:: 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:<:$	 

_output_shapes

:<: 


_output_shapes
:<:$ 

_output_shapes

:<:$ 

_output_shapes

:<: 

_output_shapes
:<:

_output_shapes
: :

_output_shapes
: :$ 

_output_shapes

:: 

_output_shapes
::$ 

_output_shapes

:<:$ 

_output_shapes

:<: 

_output_shapes
:<:$ 

_output_shapes

:<:$ 

_output_shapes

:<: 

_output_shapes
:<:

_output_shapes
: 
?
?
.__inference_sequential_12_layer_call_fn_628799

inputs
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
	unknown_5
	unknown_6
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *R
fMRK
I__inference_sequential_12_layer_call_and_return_conditional_losses_6280682
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628042
lstm_6_input
lstm_6_628022
lstm_6_628024
lstm_6_628026
lstm_7_628029
lstm_7_628031
lstm_7_628033
dense_18_628036
dense_18_628038
identity?? dense_18/StatefulPartitionedCall?lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCalllstm_6_inputlstm_6_628022lstm_6_628024lstm_6_628026*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6276272 
lstm_6/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0lstm_7_628029lstm_7_628031lstm_7_628033*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6279622 
lstm_7/StatefulPartitionedCall?
 dense_18/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0dense_18_628036dense_18_628038*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *M
fHRF
D__inference_dense_18_layer_call_and_return_conditional_losses_6280022"
 dense_18/StatefulPartitionedCall?
IdentityIdentity)dense_18/StatefulPartitionedCall:output:0!^dense_18/StatefulPartitionedCall^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2D
 dense_18/StatefulPartitionedCall dense_18/StatefulPartitionedCall2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629957

inputs/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629872*
condR
while_cond_629871*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
$__inference_signature_wrapper_628162
lstm_6_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
	unknown_5
	unknown_6
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_6_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? **
f%R#
!__inference__wrapped_model_6260982
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::22
StatefulPartitionedCallStatefulPartitionedCall:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?
?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630317

inputs
states_0
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?	
?
lstm_7_while_cond_628686*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3,
(lstm_7_while_less_lstm_7_strided_slice_1B
>lstm_7_while_lstm_7_while_cond_628686___redundant_placeholder0B
>lstm_7_while_lstm_7_while_cond_628686___redundant_placeholder1B
>lstm_7_while_lstm_7_while_cond_628686___redundant_placeholder2B
>lstm_7_while_lstm_7_while_cond_628686___redundant_placeholder3
lstm_7_while_identity
?
lstm_7/while/LessLesslstm_7_while_placeholder(lstm_7_while_less_lstm_7_strided_slice_1*
T0*
_output_shapes
: 2
lstm_7/while/Lessr
lstm_7/while/IdentityIdentitylstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_7/while/Identity"7
lstm_7_while_identitylstm_7/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?%
?
while_body_627108
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_lstm_cell_18_627132_0
while_lstm_cell_18_627134_0
while_lstm_cell_18_627136_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_lstm_cell_18_627132
while_lstm_cell_18_627134
while_lstm_cell_18_627136??*while/lstm_cell_18/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
*while/lstm_cell_18/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_18_627132_0while_lstm_cell_18_627134_0while_lstm_cell_18_627136_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6267812,
*while/lstm_cell_18/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_18/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0+^while/lstm_cell_18/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity3while/lstm_cell_18/StatefulPartitionedCall:output:1+^while/lstm_cell_18/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identity3while/lstm_cell_18/StatefulPartitionedCall:output:2+^while/lstm_cell_18/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"8
while_lstm_cell_18_627132while_lstm_cell_18_627132_0"8
while_lstm_cell_18_627134while_lstm_cell_18_627134_0"8
while_lstm_cell_18_627136while_lstm_cell_18_627136_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2X
*while/lstm_cell_18/StatefulPartitionedCall*while/lstm_cell_18/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
.__inference_sequential_12_layer_call_fn_628087
lstm_6_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
	unknown_5
	unknown_6
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_6_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *R
fMRK
I__inference_sequential_12_layer_call_and_return_conditional_losses_6280682
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::22
StatefulPartitionedCallStatefulPartitionedCall:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?
?
-__inference_lstm_cell_17_layer_call_fn_630251

inputs
states_0
states_1
unknown
	unknown_0
	unknown_1
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6262042
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity StatefulPartitionedCall:output:2^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?M
?	
lstm_7_while_body_628687*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3)
%lstm_7_while_lstm_7_strided_slice_1_0e
alstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0>
:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0@
<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0?
;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0
lstm_7_while_identity
lstm_7_while_identity_1
lstm_7_while_identity_2
lstm_7_while_identity_3
lstm_7_while_identity_4
lstm_7_while_identity_5'
#lstm_7_while_lstm_7_strided_slice_1c
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor<
8lstm_7_while_lstm_cell_18_matmul_readvariableop_resource>
:lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource=
9lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource??0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp?1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2@
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0lstm_7_while_placeholderGlstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype022
0lstm_7/while/TensorArrayV2Read/TensorListGetItem?
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype021
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp?
 lstm_7/while/lstm_cell_18/MatMulMatMul7lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:07lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2"
 lstm_7/while/lstm_cell_18/MatMul?
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype023
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
"lstm_7/while/lstm_cell_18/MatMul_1MatMullstm_7_while_placeholder_29lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2$
"lstm_7/while/lstm_cell_18/MatMul_1?
lstm_7/while/lstm_cell_18/addAddV2*lstm_7/while/lstm_cell_18/MatMul:product:0,lstm_7/while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_7/while/lstm_cell_18/add?
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype022
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?
!lstm_7/while/lstm_cell_18/BiasAddBiasAdd!lstm_7/while/lstm_cell_18/add:z:08lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2#
!lstm_7/while/lstm_cell_18/BiasAdd?
lstm_7/while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2!
lstm_7/while/lstm_cell_18/Const?
)lstm_7/while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2+
)lstm_7/while/lstm_cell_18/split/split_dim?
lstm_7/while/lstm_cell_18/splitSplit2lstm_7/while/lstm_cell_18/split/split_dim:output:0*lstm_7/while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2!
lstm_7/while/lstm_cell_18/split?
!lstm_7/while/lstm_cell_18/SigmoidSigmoid(lstm_7/while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2#
!lstm_7/while/lstm_cell_18/Sigmoid?
#lstm_7/while/lstm_cell_18/Sigmoid_1Sigmoid(lstm_7/while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2%
#lstm_7/while/lstm_cell_18/Sigmoid_1?
lstm_7/while/lstm_cell_18/mulMul'lstm_7/while/lstm_cell_18/Sigmoid_1:y:0lstm_7_while_placeholder_3*
T0*'
_output_shapes
:?????????2
lstm_7/while/lstm_cell_18/mul?
lstm_7/while/lstm_cell_18/ReluRelu(lstm_7/while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2 
lstm_7/while/lstm_cell_18/Relu?
lstm_7/while/lstm_cell_18/mul_1Mul%lstm_7/while/lstm_cell_18/Sigmoid:y:0,lstm_7/while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/mul_1?
lstm_7/while/lstm_cell_18/add_1AddV2!lstm_7/while/lstm_cell_18/mul:z:0#lstm_7/while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/add_1?
#lstm_7/while/lstm_cell_18/Sigmoid_2Sigmoid(lstm_7/while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2%
#lstm_7/while/lstm_cell_18/Sigmoid_2?
 lstm_7/while/lstm_cell_18/Relu_1Relu#lstm_7/while/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2"
 lstm_7/while/lstm_cell_18/Relu_1?
lstm_7/while/lstm_cell_18/mul_2Mul'lstm_7/while/lstm_cell_18/Sigmoid_2:y:0.lstm_7/while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/mul_2?
1lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_7_while_placeholder_1lstm_7_while_placeholder#lstm_7/while/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype023
1lstm_7/while/TensorArrayV2Write/TensorListSetItemj
lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add/y?
lstm_7/while/addAddV2lstm_7_while_placeholderlstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/addn
lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add_1/y?
lstm_7/while/add_1AddV2&lstm_7_while_lstm_7_while_loop_counterlstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/add_1?
lstm_7/while/IdentityIdentitylstm_7/while/add_1:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity?
lstm_7/while/Identity_1Identity,lstm_7_while_lstm_7_while_maximum_iterations1^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_1?
lstm_7/while/Identity_2Identitylstm_7/while/add:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_2?
lstm_7/while/Identity_3IdentityAlstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_3?
lstm_7/while/Identity_4Identity#lstm_7/while/lstm_cell_18/mul_2:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_7/while/Identity_4?
lstm_7/while/Identity_5Identity#lstm_7/while/lstm_cell_18/add_1:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_7/while/Identity_5"7
lstm_7_while_identitylstm_7/while/Identity:output:0";
lstm_7_while_identity_1 lstm_7/while/Identity_1:output:0";
lstm_7_while_identity_2 lstm_7/while/Identity_2:output:0";
lstm_7_while_identity_3 lstm_7/while/Identity_3:output:0";
lstm_7_while_identity_4 lstm_7/while/Identity_4:output:0";
lstm_7_while_identity_5 lstm_7/while/Identity_5:output:0"L
#lstm_7_while_lstm_7_strided_slice_1%lstm_7_while_lstm_7_strided_slice_1_0"x
9lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0"z
:lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0"v
8lstm_7_while_lstm_cell_18_matmul_readvariableop_resource:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0"?
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensoralstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2d
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp2b
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2f
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_629871
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629871___redundant_placeholder04
0while_while_cond_629871___redundant_placeholder14
0while_while_cond_629871___redundant_placeholder24
0while_while_cond_629871___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_627962

inputs/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627877*
condR
while_cond_627876*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
while_cond_628887
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_628887___redundant_placeholder04
0while_while_cond_628887___redundant_placeholder14
0while_while_cond_628887___redundant_placeholder24
0while_while_cond_628887___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628112

inputs
lstm_6_628092
lstm_6_628094
lstm_6_628096
lstm_7_628099
lstm_7_628101
lstm_7_628103
dense_18_628106
dense_18_628108
identity?? dense_18/StatefulPartitionedCall?lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCallinputslstm_6_628092lstm_6_628094lstm_6_628096*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6276272 
lstm_6/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0lstm_7_628099lstm_7_628101lstm_7_628103*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6279622 
lstm_7/StatefulPartitionedCall?
 dense_18/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0dense_18_628106dense_18_628108*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *M
fHRF
D__inference_dense_18_layer_call_and_return_conditional_losses_6280022"
 dense_18/StatefulPartitionedCall?
IdentityIdentity)dense_18/StatefulPartitionedCall:output:0!^dense_18/StatefulPartitionedCall^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2D
 dense_18/StatefulPartitionedCall dense_18/StatefulPartitionedCall2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
-__inference_lstm_cell_18_layer_call_fn_630351

inputs
states_0
states_1
unknown
	unknown_0
	unknown_1
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6268142
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity StatefulPartitionedCall:output:2^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?B
?
while_body_627542
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_629543
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629543___redundant_placeholder04
0while_while_cond_629543___redundant_placeholder14
0while_while_cond_629543___redundant_placeholder24
0while_while_cond_629543___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630217

inputs
states_0
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?
?
'__inference_lstm_7_layer_call_fn_629804
inputs_0
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6273092
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?d
?
"__inference__traced_restore_630522
file_prefix$
 assignvariableop_dense_18_kernel$
 assignvariableop_1_dense_18_bias#
assignvariableop_2_rmsprop_iter$
 assignvariableop_3_rmsprop_decay,
(assignvariableop_4_rmsprop_learning_rate'
#assignvariableop_5_rmsprop_momentum"
assignvariableop_6_rmsprop_rho1
-assignvariableop_7_lstm_6_lstm_cell_17_kernel;
7assignvariableop_8_lstm_6_lstm_cell_17_recurrent_kernel/
+assignvariableop_9_lstm_6_lstm_cell_17_bias2
.assignvariableop_10_lstm_7_lstm_cell_18_kernel<
8assignvariableop_11_lstm_7_lstm_cell_18_recurrent_kernel0
,assignvariableop_12_lstm_7_lstm_cell_18_bias
assignvariableop_13_total
assignvariableop_14_count3
/assignvariableop_15_rmsprop_dense_18_kernel_rms1
-assignvariableop_16_rmsprop_dense_18_bias_rms>
:assignvariableop_17_rmsprop_lstm_6_lstm_cell_17_kernel_rmsH
Dassignvariableop_18_rmsprop_lstm_6_lstm_cell_17_recurrent_kernel_rms<
8assignvariableop_19_rmsprop_lstm_6_lstm_cell_17_bias_rms>
:assignvariableop_20_rmsprop_lstm_7_lstm_cell_18_kernel_rmsH
Dassignvariableop_21_rmsprop_lstm_7_lstm_cell_18_recurrent_kernel_rms<
8assignvariableop_22_rmsprop_lstm_7_lstm_cell_18_bias_rms
identity_24??AssignVariableOp?AssignVariableOp_1?AssignVariableOp_10?AssignVariableOp_11?AssignVariableOp_12?AssignVariableOp_13?AssignVariableOp_14?AssignVariableOp_15?AssignVariableOp_16?AssignVariableOp_17?AssignVariableOp_18?AssignVariableOp_19?AssignVariableOp_2?AssignVariableOp_20?AssignVariableOp_21?AssignVariableOp_22?AssignVariableOp_3?AssignVariableOp_4?AssignVariableOp_5?AssignVariableOp_6?AssignVariableOp_7?AssignVariableOp_8?AssignVariableOp_9?
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:*
dtype0*?

value?
B?
B6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB-optimizer/momentum/.ATTRIBUTES/VARIABLE_VALUEB(optimizer/rho/.ATTRIBUTES/VARIABLE_VALUEB&variables/0/.ATTRIBUTES/VARIABLE_VALUEB&variables/1/.ATTRIBUTES/VARIABLE_VALUEB&variables/2/.ATTRIBUTES/VARIABLE_VALUEB&variables/3/.ATTRIBUTES/VARIABLE_VALUEB&variables/4/.ATTRIBUTES/VARIABLE_VALUEB&variables/5/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBTlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/0/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/1/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/2/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/3/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/4/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEBDvariables/5/.OPTIMIZER_SLOT/optimizer/rms/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPH2
RestoreV2/tensor_names?
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:*
dtype0*C
value:B8B B B B B B B B B B B B B B B B B B B B B B B B 2
RestoreV2/shape_and_slices?
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*t
_output_shapesb
`::::::::::::::::::::::::*&
dtypes
2	2
	RestoreV2g
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:2

Identity?
AssignVariableOpAssignVariableOp assignvariableop_dense_18_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOpk

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:2

Identity_1?
AssignVariableOp_1AssignVariableOp assignvariableop_1_dense_18_biasIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_1k

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0	*
_output_shapes
:2

Identity_2?
AssignVariableOp_2AssignVariableOpassignvariableop_2_rmsprop_iterIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	2
AssignVariableOp_2k

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:2

Identity_3?
AssignVariableOp_3AssignVariableOp assignvariableop_3_rmsprop_decayIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_3k

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:2

Identity_4?
AssignVariableOp_4AssignVariableOp(assignvariableop_4_rmsprop_learning_rateIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_4k

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:2

Identity_5?
AssignVariableOp_5AssignVariableOp#assignvariableop_5_rmsprop_momentumIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_5k

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:2

Identity_6?
AssignVariableOp_6AssignVariableOpassignvariableop_6_rmsprop_rhoIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_6k

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:2

Identity_7?
AssignVariableOp_7AssignVariableOp-assignvariableop_7_lstm_6_lstm_cell_17_kernelIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_7k

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:2

Identity_8?
AssignVariableOp_8AssignVariableOp7assignvariableop_8_lstm_6_lstm_cell_17_recurrent_kernelIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_8k

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:2

Identity_9?
AssignVariableOp_9AssignVariableOp+assignvariableop_9_lstm_6_lstm_cell_17_biasIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_9n
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:2
Identity_10?
AssignVariableOp_10AssignVariableOp.assignvariableop_10_lstm_7_lstm_cell_18_kernelIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_10n
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:2
Identity_11?
AssignVariableOp_11AssignVariableOp8assignvariableop_11_lstm_7_lstm_cell_18_recurrent_kernelIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_11n
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
:2
Identity_12?
AssignVariableOp_12AssignVariableOp,assignvariableop_12_lstm_7_lstm_cell_18_biasIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_12n
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:2
Identity_13?
AssignVariableOp_13AssignVariableOpassignvariableop_13_totalIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_13n
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:2
Identity_14?
AssignVariableOp_14AssignVariableOpassignvariableop_14_countIdentity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_14n
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:2
Identity_15?
AssignVariableOp_15AssignVariableOp/assignvariableop_15_rmsprop_dense_18_kernel_rmsIdentity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_15n
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:2
Identity_16?
AssignVariableOp_16AssignVariableOp-assignvariableop_16_rmsprop_dense_18_bias_rmsIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_16n
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:2
Identity_17?
AssignVariableOp_17AssignVariableOp:assignvariableop_17_rmsprop_lstm_6_lstm_cell_17_kernel_rmsIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_17n
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0*
_output_shapes
:2
Identity_18?
AssignVariableOp_18AssignVariableOpDassignvariableop_18_rmsprop_lstm_6_lstm_cell_17_recurrent_kernel_rmsIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_18n
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:2
Identity_19?
AssignVariableOp_19AssignVariableOp8assignvariableop_19_rmsprop_lstm_6_lstm_cell_17_bias_rmsIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_19n
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:2
Identity_20?
AssignVariableOp_20AssignVariableOp:assignvariableop_20_rmsprop_lstm_7_lstm_cell_18_kernel_rmsIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_20n
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:2
Identity_21?
AssignVariableOp_21AssignVariableOpDassignvariableop_21_rmsprop_lstm_7_lstm_cell_18_recurrent_kernel_rmsIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_21n
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:2
Identity_22?
AssignVariableOp_22AssignVariableOp8assignvariableop_22_rmsprop_lstm_7_lstm_cell_18_bias_rmsIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype02
AssignVariableOp_229
NoOpNoOp"/device:CPU:0*
_output_shapes
 2
NoOp?
Identity_23Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: 2
Identity_23?
Identity_24IdentityIdentity_23:output:0^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_3^AssignVariableOp_4^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*
T0*
_output_shapes
: 2
Identity_24"#
identity_24Identity_24:output:0*q
_input_shapes`
^: :::::::::::::::::::::::2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222(
AssignVariableOp_3AssignVariableOp_32(
AssignVariableOp_4AssignVariableOp_42(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
?B
?
while_body_627389
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
'__inference_lstm_6_layer_call_fn_629148
inputs_0
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :??????????????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6266992
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?
?
-__inference_lstm_cell_18_layer_call_fn_630334

inputs
states_0
states_1
unknown
	unknown_0
	unknown_1
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6267812
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity StatefulPartitionedCall:output:2^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?D
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_626567

inputs
lstm_cell_17_626485
lstm_cell_17_626487
lstm_cell_17_626489
identity??$lstm_cell_17/StatefulPartitionedCall?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
$lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0zeros_1:output:0lstm_cell_17_626485lstm_cell_17_626487lstm_cell_17_626489*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6261712&
$lstm_cell_17/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_17_626485lstm_cell_17_626487lstm_cell_17_626489*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_626498*
condR
while_cond_626497*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0%^lstm_cell_17/StatefulPartitionedCall^while*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2L
$lstm_cell_17/StatefulPartitionedCall$lstm_cell_17/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :??????????????????
 
_user_specified_nameinputs
?
?
while_cond_626497
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_626497___redundant_placeholder04
0while_while_cond_626497___redundant_placeholder14
0while_while_cond_626497___redundant_placeholder24
0while_while_cond_626497___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628019
lstm_6_input
lstm_6_627650
lstm_6_627652
lstm_6_627654
lstm_7_627985
lstm_7_627987
lstm_7_627989
dense_18_628013
dense_18_628015
identity?? dense_18/StatefulPartitionedCall?lstm_6/StatefulPartitionedCall?lstm_7/StatefulPartitionedCall?
lstm_6/StatefulPartitionedCallStatefulPartitionedCalllstm_6_inputlstm_6_627650lstm_6_627652lstm_6_627654*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6274742 
lstm_6/StatefulPartitionedCall?
lstm_7/StatefulPartitionedCallStatefulPartitionedCall'lstm_6/StatefulPartitionedCall:output:0lstm_7_627985lstm_7_627987lstm_7_627989*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6278092 
lstm_7/StatefulPartitionedCall?
 dense_18/StatefulPartitionedCallStatefulPartitionedCall'lstm_7/StatefulPartitionedCall:output:0dense_18_628013dense_18_628015*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *M
fHRF
D__inference_dense_18_layer_call_and_return_conditional_losses_6280022"
 dense_18/StatefulPartitionedCall?
IdentityIdentity)dense_18/StatefulPartitionedCall:output:0!^dense_18/StatefulPartitionedCall^lstm_6/StatefulPartitionedCall^lstm_7/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2D
 dense_18/StatefulPartitionedCall dense_18/StatefulPartitionedCall2@
lstm_6/StatefulPartitionedCalllstm_6/StatefulPartitionedCall2@
lstm_7/StatefulPartitionedCalllstm_7/StatefulPartitionedCall:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?
~
)__inference_dense_18_layer_call_fn_630151

inputs
unknown
	unknown_0
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*$
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *M
fHRF
D__inference_dense_18_layer_call_and_return_conditional_losses_6280022
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
&sequential_12_lstm_6_while_cond_625857F
Bsequential_12_lstm_6_while_sequential_12_lstm_6_while_loop_counterL
Hsequential_12_lstm_6_while_sequential_12_lstm_6_while_maximum_iterations*
&sequential_12_lstm_6_while_placeholder,
(sequential_12_lstm_6_while_placeholder_1,
(sequential_12_lstm_6_while_placeholder_2,
(sequential_12_lstm_6_while_placeholder_3H
Dsequential_12_lstm_6_while_less_sequential_12_lstm_6_strided_slice_1^
Zsequential_12_lstm_6_while_sequential_12_lstm_6_while_cond_625857___redundant_placeholder0^
Zsequential_12_lstm_6_while_sequential_12_lstm_6_while_cond_625857___redundant_placeholder1^
Zsequential_12_lstm_6_while_sequential_12_lstm_6_while_cond_625857___redundant_placeholder2^
Zsequential_12_lstm_6_while_sequential_12_lstm_6_while_cond_625857___redundant_placeholder3'
#sequential_12_lstm_6_while_identity
?
sequential_12/lstm_6/while/LessLess&sequential_12_lstm_6_while_placeholderDsequential_12_lstm_6_while_less_sequential_12_lstm_6_strided_slice_1*
T0*
_output_shapes
: 2!
sequential_12/lstm_6/while/Less?
#sequential_12/lstm_6/while/IdentityIdentity#sequential_12/lstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2%
#sequential_12/lstm_6/while/Identity"S
#sequential_12_lstm_6_while_identity,sequential_12/lstm_6/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?B
?
while_body_627877
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_630024
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_630024___redundant_placeholder04
0while_while_cond_630024___redundant_placeholder14
0while_while_cond_630024___redundant_placeholder24
0while_while_cond_630024___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?	
?
lstm_7_while_cond_628378*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3,
(lstm_7_while_less_lstm_7_strided_slice_1B
>lstm_7_while_lstm_7_while_cond_628378___redundant_placeholder0B
>lstm_7_while_lstm_7_while_cond_628378___redundant_placeholder1B
>lstm_7_while_lstm_7_while_cond_628378___redundant_placeholder2B
>lstm_7_while_lstm_7_while_cond_628378___redundant_placeholder3
lstm_7_while_identity
?
lstm_7/while/LessLesslstm_7_while_placeholder(lstm_7_while_less_lstm_7_strided_slice_1*
T0*
_output_shapes
: 2
lstm_7/while/Lessr
lstm_7/while/IdentityIdentitylstm_7/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_7/while/Identity"7
lstm_7_while_identitylstm_7/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?D
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_627309

inputs
lstm_cell_18_627227
lstm_cell_18_627229
lstm_cell_18_627231
identity??$lstm_cell_18/StatefulPartitionedCall?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
$lstm_cell_18/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0zeros_1:output:0lstm_cell_18_627227lstm_cell_18_627229lstm_cell_18_627231*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6268142&
$lstm_cell_18/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_18_627227lstm_cell_18_627229lstm_cell_18_627231*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627240*
condR
while_cond_627239*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0%^lstm_cell_18/StatefulPartitionedCall^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2L
$lstm_cell_18/StatefulPartitionedCall$lstm_cell_18/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :??????????????????
 
_user_specified_nameinputs
?
?
while_cond_627107
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627107___redundant_placeholder04
0while_while_cond_627107___redundant_placeholder14
0while_while_cond_627107___redundant_placeholder24
0while_while_cond_627107___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
while_cond_627388
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627388___redundant_placeholder04
0while_while_cond_627388___redundant_placeholder14
0while_while_cond_627388___redundant_placeholder24
0while_while_cond_627388___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
while_cond_629040
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629040___redundant_placeholder04
0while_while_cond_629040___redundant_placeholder14
0while_while_cond_629040___redundant_placeholder24
0while_while_cond_629040___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?b
?
&sequential_12_lstm_6_while_body_625858F
Bsequential_12_lstm_6_while_sequential_12_lstm_6_while_loop_counterL
Hsequential_12_lstm_6_while_sequential_12_lstm_6_while_maximum_iterations*
&sequential_12_lstm_6_while_placeholder,
(sequential_12_lstm_6_while_placeholder_1,
(sequential_12_lstm_6_while_placeholder_2,
(sequential_12_lstm_6_while_placeholder_3E
Asequential_12_lstm_6_while_sequential_12_lstm_6_strided_slice_1_0?
}sequential_12_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_6_tensorarrayunstack_tensorlistfromtensor_0L
Hsequential_12_lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0N
Jsequential_12_lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0M
Isequential_12_lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0'
#sequential_12_lstm_6_while_identity)
%sequential_12_lstm_6_while_identity_1)
%sequential_12_lstm_6_while_identity_2)
%sequential_12_lstm_6_while_identity_3)
%sequential_12_lstm_6_while_identity_4)
%sequential_12_lstm_6_while_identity_5C
?sequential_12_lstm_6_while_sequential_12_lstm_6_strided_slice_1
{sequential_12_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_6_tensorarrayunstack_tensorlistfromtensorJ
Fsequential_12_lstm_6_while_lstm_cell_17_matmul_readvariableop_resourceL
Hsequential_12_lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resourceK
Gsequential_12_lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource??>sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?=sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp??sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
Lsequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2N
Lsequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
>sequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItem}sequential_12_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_6_tensorarrayunstack_tensorlistfromtensor_0&sequential_12_lstm_6_while_placeholderUsequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02@
>sequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItem?
=sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOpHsequential_12_lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02?
=sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp?
.sequential_12/lstm_6/while/lstm_cell_17/MatMulMatMulEsequential_12/lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:0Esequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<20
.sequential_12/lstm_6/while/lstm_cell_17/MatMul?
?sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOpJsequential_12_lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02A
?sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
0sequential_12/lstm_6/while/lstm_cell_17/MatMul_1MatMul(sequential_12_lstm_6_while_placeholder_2Gsequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<22
0sequential_12/lstm_6/while/lstm_cell_17/MatMul_1?
+sequential_12/lstm_6/while/lstm_cell_17/addAddV28sequential_12/lstm_6/while/lstm_cell_17/MatMul:product:0:sequential_12/lstm_6/while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2-
+sequential_12/lstm_6/while/lstm_cell_17/add?
>sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOpIsequential_12_lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02@
>sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?
/sequential_12/lstm_6/while/lstm_cell_17/BiasAddBiasAdd/sequential_12/lstm_6/while/lstm_cell_17/add:z:0Fsequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<21
/sequential_12/lstm_6/while/lstm_cell_17/BiasAdd?
-sequential_12/lstm_6/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2/
-sequential_12/lstm_6/while/lstm_cell_17/Const?
7sequential_12/lstm_6/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :29
7sequential_12/lstm_6/while/lstm_cell_17/split/split_dim?
-sequential_12/lstm_6/while/lstm_cell_17/splitSplit@sequential_12/lstm_6/while/lstm_cell_17/split/split_dim:output:08sequential_12/lstm_6/while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2/
-sequential_12/lstm_6/while/lstm_cell_17/split?
/sequential_12/lstm_6/while/lstm_cell_17/SigmoidSigmoid6sequential_12/lstm_6/while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????21
/sequential_12/lstm_6/while/lstm_cell_17/Sigmoid?
1sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_1Sigmoid6sequential_12/lstm_6/while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????23
1sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_1?
+sequential_12/lstm_6/while/lstm_cell_17/mulMul5sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_1:y:0(sequential_12_lstm_6_while_placeholder_3*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_6/while/lstm_cell_17/mul?
,sequential_12/lstm_6/while/lstm_cell_17/ReluRelu6sequential_12/lstm_6/while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2.
,sequential_12/lstm_6/while/lstm_cell_17/Relu?
-sequential_12/lstm_6/while/lstm_cell_17/mul_1Mul3sequential_12/lstm_6/while/lstm_cell_17/Sigmoid:y:0:sequential_12/lstm_6/while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_6/while/lstm_cell_17/mul_1?
-sequential_12/lstm_6/while/lstm_cell_17/add_1AddV2/sequential_12/lstm_6/while/lstm_cell_17/mul:z:01sequential_12/lstm_6/while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_6/while/lstm_cell_17/add_1?
1sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_2Sigmoid6sequential_12/lstm_6/while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????23
1sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_2?
.sequential_12/lstm_6/while/lstm_cell_17/Relu_1Relu1sequential_12/lstm_6/while/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????20
.sequential_12/lstm_6/while/lstm_cell_17/Relu_1?
-sequential_12/lstm_6/while/lstm_cell_17/mul_2Mul5sequential_12/lstm_6/while/lstm_cell_17/Sigmoid_2:y:0<sequential_12/lstm_6/while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2/
-sequential_12/lstm_6/while/lstm_cell_17/mul_2?
?sequential_12/lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItem(sequential_12_lstm_6_while_placeholder_1&sequential_12_lstm_6_while_placeholder1sequential_12/lstm_6/while/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02A
?sequential_12/lstm_6/while/TensorArrayV2Write/TensorListSetItem?
 sequential_12/lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_12/lstm_6/while/add/y?
sequential_12/lstm_6/while/addAddV2&sequential_12_lstm_6_while_placeholder)sequential_12/lstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2 
sequential_12/lstm_6/while/add?
"sequential_12/lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_12/lstm_6/while/add_1/y?
 sequential_12/lstm_6/while/add_1AddV2Bsequential_12_lstm_6_while_sequential_12_lstm_6_while_loop_counter+sequential_12/lstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2"
 sequential_12/lstm_6/while/add_1?
#sequential_12/lstm_6/while/IdentityIdentity$sequential_12/lstm_6/while/add_1:z:0?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2%
#sequential_12/lstm_6/while/Identity?
%sequential_12/lstm_6/while/Identity_1IdentityHsequential_12_lstm_6_while_sequential_12_lstm_6_while_maximum_iterations?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_6/while/Identity_1?
%sequential_12/lstm_6/while/Identity_2Identity"sequential_12/lstm_6/while/add:z:0?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_6/while/Identity_2?
%sequential_12/lstm_6/while/Identity_3IdentityOsequential_12/lstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:0?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2'
%sequential_12/lstm_6/while/Identity_3?
%sequential_12/lstm_6/while/Identity_4Identity1sequential_12/lstm_6/while/lstm_cell_17/mul_2:z:0?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_6/while/Identity_4?
%sequential_12/lstm_6/while/Identity_5Identity1sequential_12/lstm_6/while/lstm_cell_17/add_1:z:0?^sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>^sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp@^sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_6/while/Identity_5"S
#sequential_12_lstm_6_while_identity,sequential_12/lstm_6/while/Identity:output:0"W
%sequential_12_lstm_6_while_identity_1.sequential_12/lstm_6/while/Identity_1:output:0"W
%sequential_12_lstm_6_while_identity_2.sequential_12/lstm_6/while/Identity_2:output:0"W
%sequential_12_lstm_6_while_identity_3.sequential_12/lstm_6/while/Identity_3:output:0"W
%sequential_12_lstm_6_while_identity_4.sequential_12/lstm_6/while/Identity_4:output:0"W
%sequential_12_lstm_6_while_identity_5.sequential_12/lstm_6/while/Identity_5:output:0"?
Gsequential_12_lstm_6_while_lstm_cell_17_biasadd_readvariableop_resourceIsequential_12_lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0"?
Hsequential_12_lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resourceJsequential_12_lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0"?
Fsequential_12_lstm_6_while_lstm_cell_17_matmul_readvariableop_resourceHsequential_12_lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0"?
?sequential_12_lstm_6_while_sequential_12_lstm_6_strided_slice_1Asequential_12_lstm_6_while_sequential_12_lstm_6_strided_slice_1_0"?
{sequential_12_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_6_tensorarrayunstack_tensorlistfromtensor}sequential_12_lstm_6_while_tensorarrayv2read_tensorlistgetitem_sequential_12_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2?
>sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp>sequential_12/lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp2~
=sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp=sequential_12/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2?
?sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?sequential_12/lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?M
?	
lstm_7_while_body_628379*
&lstm_7_while_lstm_7_while_loop_counter0
,lstm_7_while_lstm_7_while_maximum_iterations
lstm_7_while_placeholder
lstm_7_while_placeholder_1
lstm_7_while_placeholder_2
lstm_7_while_placeholder_3)
%lstm_7_while_lstm_7_strided_slice_1_0e
alstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0>
:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0@
<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0?
;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0
lstm_7_while_identity
lstm_7_while_identity_1
lstm_7_while_identity_2
lstm_7_while_identity_3
lstm_7_while_identity_4
lstm_7_while_identity_5'
#lstm_7_while_lstm_7_strided_slice_1c
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor<
8lstm_7_while_lstm_cell_18_matmul_readvariableop_resource>
:lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource=
9lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource??0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp?1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2@
>lstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_7/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0lstm_7_while_placeholderGlstm_7/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype022
0lstm_7/while/TensorArrayV2Read/TensorListGetItem?
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype021
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp?
 lstm_7/while/lstm_cell_18/MatMulMatMul7lstm_7/while/TensorArrayV2Read/TensorListGetItem:item:07lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2"
 lstm_7/while/lstm_cell_18/MatMul?
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype023
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp?
"lstm_7/while/lstm_cell_18/MatMul_1MatMullstm_7_while_placeholder_29lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2$
"lstm_7/while/lstm_cell_18/MatMul_1?
lstm_7/while/lstm_cell_18/addAddV2*lstm_7/while/lstm_cell_18/MatMul:product:0,lstm_7/while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_7/while/lstm_cell_18/add?
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype022
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp?
!lstm_7/while/lstm_cell_18/BiasAddBiasAdd!lstm_7/while/lstm_cell_18/add:z:08lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2#
!lstm_7/while/lstm_cell_18/BiasAdd?
lstm_7/while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2!
lstm_7/while/lstm_cell_18/Const?
)lstm_7/while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2+
)lstm_7/while/lstm_cell_18/split/split_dim?
lstm_7/while/lstm_cell_18/splitSplit2lstm_7/while/lstm_cell_18/split/split_dim:output:0*lstm_7/while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2!
lstm_7/while/lstm_cell_18/split?
!lstm_7/while/lstm_cell_18/SigmoidSigmoid(lstm_7/while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2#
!lstm_7/while/lstm_cell_18/Sigmoid?
#lstm_7/while/lstm_cell_18/Sigmoid_1Sigmoid(lstm_7/while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2%
#lstm_7/while/lstm_cell_18/Sigmoid_1?
lstm_7/while/lstm_cell_18/mulMul'lstm_7/while/lstm_cell_18/Sigmoid_1:y:0lstm_7_while_placeholder_3*
T0*'
_output_shapes
:?????????2
lstm_7/while/lstm_cell_18/mul?
lstm_7/while/lstm_cell_18/ReluRelu(lstm_7/while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2 
lstm_7/while/lstm_cell_18/Relu?
lstm_7/while/lstm_cell_18/mul_1Mul%lstm_7/while/lstm_cell_18/Sigmoid:y:0,lstm_7/while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/mul_1?
lstm_7/while/lstm_cell_18/add_1AddV2!lstm_7/while/lstm_cell_18/mul:z:0#lstm_7/while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/add_1?
#lstm_7/while/lstm_cell_18/Sigmoid_2Sigmoid(lstm_7/while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2%
#lstm_7/while/lstm_cell_18/Sigmoid_2?
 lstm_7/while/lstm_cell_18/Relu_1Relu#lstm_7/while/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2"
 lstm_7/while/lstm_cell_18/Relu_1?
lstm_7/while/lstm_cell_18/mul_2Mul'lstm_7/while/lstm_cell_18/Sigmoid_2:y:0.lstm_7/while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_7/while/lstm_cell_18/mul_2?
1lstm_7/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_7_while_placeholder_1lstm_7_while_placeholder#lstm_7/while/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype023
1lstm_7/while/TensorArrayV2Write/TensorListSetItemj
lstm_7/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add/y?
lstm_7/while/addAddV2lstm_7_while_placeholderlstm_7/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/addn
lstm_7/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_7/while/add_1/y?
lstm_7/while/add_1AddV2&lstm_7_while_lstm_7_while_loop_counterlstm_7/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_7/while/add_1?
lstm_7/while/IdentityIdentitylstm_7/while/add_1:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity?
lstm_7/while/Identity_1Identity,lstm_7_while_lstm_7_while_maximum_iterations1^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_1?
lstm_7/while/Identity_2Identitylstm_7/while/add:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_2?
lstm_7/while/Identity_3IdentityAlstm_7/while/TensorArrayV2Write/TensorListSetItem:output_handle:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_7/while/Identity_3?
lstm_7/while/Identity_4Identity#lstm_7/while/lstm_cell_18/mul_2:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_7/while/Identity_4?
lstm_7/while/Identity_5Identity#lstm_7/while/lstm_cell_18/add_1:z:01^lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0^lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2^lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_7/while/Identity_5"7
lstm_7_while_identitylstm_7/while/Identity:output:0";
lstm_7_while_identity_1 lstm_7/while/Identity_1:output:0";
lstm_7_while_identity_2 lstm_7/while/Identity_2:output:0";
lstm_7_while_identity_3 lstm_7/while/Identity_3:output:0";
lstm_7_while_identity_4 lstm_7/while/Identity_4:output:0";
lstm_7_while_identity_5 lstm_7/while/Identity_5:output:0"L
#lstm_7_while_lstm_7_strided_slice_1%lstm_7_while_lstm_7_strided_slice_1_0"x
9lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource;lstm_7_while_lstm_cell_18_biasadd_readvariableop_resource_0"z
:lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource<lstm_7_while_lstm_cell_18_matmul_1_readvariableop_resource_0"v
8lstm_7_while_lstm_cell_18_matmul_readvariableop_resource:lstm_7_while_lstm_cell_18_matmul_readvariableop_resource_0"?
_lstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensoralstm_7_while_tensorarrayv2read_tensorlistgetitem_lstm_7_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2d
0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp0lstm_7/while/lstm_cell_18/BiasAdd/ReadVariableOp2b
/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp/lstm_7/while/lstm_cell_18/MatMul/ReadVariableOp2f
1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp1lstm_7/while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_627239
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627239___redundant_placeholder04
0while_while_cond_627239___redundant_placeholder14
0while_while_cond_627239___redundant_placeholder24
0while_while_cond_627239___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?	
?
lstm_6_while_cond_628229*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3,
(lstm_6_while_less_lstm_6_strided_slice_1B
>lstm_6_while_lstm_6_while_cond_628229___redundant_placeholder0B
>lstm_6_while_lstm_6_while_cond_628229___redundant_placeholder1B
>lstm_6_while_lstm_6_while_cond_628229___redundant_placeholder2B
>lstm_6_while_lstm_6_while_cond_628229___redundant_placeholder3
lstm_6_while_identity
?
lstm_6/while/LessLesslstm_6_while_placeholder(lstm_6_while_less_lstm_6_strided_slice_1*
T0*
_output_shapes
: 2
lstm_6/while/Lessr
lstm_6/while/IdentityIdentitylstm_6/while/Less:z:0*
T0
*
_output_shapes
: 2
lstm_6/while/Identity"7
lstm_6_while_identitylstm_6/while/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629629
inputs_0/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629544*
condR
while_cond_629543*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?
?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630284

inputs
states_0
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?%
?
while_body_626498
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0
while_lstm_cell_17_626522_0
while_lstm_cell_17_626524_0
while_lstm_cell_17_626526_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor
while_lstm_cell_17_626522
while_lstm_cell_17_626524
while_lstm_cell_17_626526??*while/lstm_cell_17/StatefulPartitionedCall?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
*while/lstm_cell_17/StatefulPartitionedCallStatefulPartitionedCall0while/TensorArrayV2Read/TensorListGetItem:item:0while_placeholder_2while_placeholder_3while_lstm_cell_17_626522_0while_lstm_cell_17_626524_0while_lstm_cell_17_626526_0*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6261712,
*while/lstm_cell_17/StatefulPartitionedCall?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholder3while/lstm_cell_17/StatefulPartitionedCall:output:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0+^while/lstm_cell_17/StatefulPartitionedCall*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identity3while/lstm_cell_17/StatefulPartitionedCall:output:1+^while/lstm_cell_17/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identity3while/lstm_cell_17/StatefulPartitionedCall:output:2+^while/lstm_cell_17/StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"8
while_lstm_cell_17_626522while_lstm_cell_17_626522_0"8
while_lstm_cell_17_626524while_lstm_cell_17_626524_0"8
while_lstm_cell_17_626526while_lstm_cell_17_626526_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2X
*while/lstm_cell_17/StatefulPartitionedCall*while/lstm_cell_17/StatefulPartitionedCall: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?B
?
while_body_627724
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630184

inputs
states_0
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOp{
MatMul_1MatMulstates_0MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?
?
'__inference_lstm_6_layer_call_fn_629137
inputs_0
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputs_0unknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *4
_output_shapes"
 :??????????????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6265672
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::22
StatefulPartitionedCallStatefulPartitionedCall:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?B
?
while_body_628888
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?B
?
while_body_630025
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
while_cond_629215
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_629215___redundant_placeholder04
0while_while_cond_629215___redundant_placeholder14
0while_while_cond_629215___redundant_placeholder24
0while_while_cond_629215___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?
?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_626814

inputs

states
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_namestates:OK
'
_output_shapes
:?????????
 
_user_specified_namestates
?
?
'__inference_lstm_7_layer_call_fn_630132

inputs
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_7_layer_call_and_return_conditional_losses_6279622
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_627474

inputs/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627389*
condR
while_cond_627388*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?
?
.__inference_sequential_12_layer_call_fn_628131
lstm_6_input
unknown
	unknown_0
	unknown_1
	unknown_2
	unknown_3
	unknown_4
	unknown_5
	unknown_6
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCalllstm_6_inputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6*
Tin
2	*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:?????????**
_read_only_resource_inputs

*-
config_proto

CPU

GPU 2J 8? *R
fMRK
I__inference_sequential_12_layer_call_and_return_conditional_losses_6281122
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::22
StatefulPartitionedCallStatefulPartitionedCall:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?
?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_626204

inputs

states
states_1"
matmul_readvariableop_resource$
 matmul_1_readvariableop_resource#
biasadd_readvariableop_resource
identity

identity_1

identity_2??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?MatMul_1/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
MatMul?
MatMul_1/ReadVariableOpReadVariableOp matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02
MatMul_1/ReadVariableOpy
MatMul_1MatMulstatesMatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2

MatMul_1k
addAddV2MatMul:product:0MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
add?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:<*
dtype02
BiasAdd/ReadVariableOpx
BiasAddBiasAddadd:z:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2	
BiasAddP
ConstConst*
_output_shapes
: *
dtype0*
value	B :2
Constd
split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
split/split_dim?
splitSplitsplit/split_dim:output:0BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
split_
SigmoidSigmoidsplit:output:0*
T0*'
_output_shapes
:?????????2	
Sigmoidc
	Sigmoid_1Sigmoidsplit:output:1*
T0*'
_output_shapes
:?????????2
	Sigmoid_1\
mulMulSigmoid_1:y:0states_1*
T0*'
_output_shapes
:?????????2
mulV
ReluRelusplit:output:2*
T0*'
_output_shapes
:?????????2
Reluh
mul_1MulSigmoid:y:0Relu:activations:0*
T0*'
_output_shapes
:?????????2
mul_1]
add_1AddV2mul:z:0	mul_1:z:0*
T0*'
_output_shapes
:?????????2
add_1c
	Sigmoid_2Sigmoidsplit:output:3*
T0*'
_output_shapes
:?????????2
	Sigmoid_2U
Relu_1Relu	add_1:z:0*
T0*'
_output_shapes
:?????????2
Relu_1l
mul_2MulSigmoid_2:y:0Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
mul_2?
IdentityIdentity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity	mul_2:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity	add_1:z:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp^MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp22
MatMul_1/ReadVariableOpMatMul_1/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:OK
'
_output_shapes
:?????????
 
_user_specified_namestates:OK
'
_output_shapes
:?????????
 
_user_specified_namestates
?B
?
while_body_629697
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629782
inputs_0/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629697*
condR
while_cond_629696*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_628973
inputs_0/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_628888*
condR
while_cond_628887*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
Ď
?
!__inference__wrapped_model_626098
lstm_6_inputD
@sequential_12_lstm_6_lstm_cell_17_matmul_readvariableop_resourceF
Bsequential_12_lstm_6_lstm_cell_17_matmul_1_readvariableop_resourceE
Asequential_12_lstm_6_lstm_cell_17_biasadd_readvariableop_resourceD
@sequential_12_lstm_7_lstm_cell_18_matmul_readvariableop_resourceF
Bsequential_12_lstm_7_lstm_cell_18_matmul_1_readvariableop_resourceE
Asequential_12_lstm_7_lstm_cell_18_biasadd_readvariableop_resource9
5sequential_12_dense_18_matmul_readvariableop_resource:
6sequential_12_dense_18_biasadd_readvariableop_resource
identity??-sequential_12/dense_18/BiasAdd/ReadVariableOp?,sequential_12/dense_18/MatMul/ReadVariableOp?8sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?7sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp?9sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?sequential_12/lstm_6/while?8sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?7sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp?9sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?sequential_12/lstm_7/whilet
sequential_12/lstm_6/ShapeShapelstm_6_input*
T0*
_output_shapes
:2
sequential_12/lstm_6/Shape?
(sequential_12/lstm_6/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_12/lstm_6/strided_slice/stack?
*sequential_12/lstm_6/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_12/lstm_6/strided_slice/stack_1?
*sequential_12/lstm_6/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_12/lstm_6/strided_slice/stack_2?
"sequential_12/lstm_6/strided_sliceStridedSlice#sequential_12/lstm_6/Shape:output:01sequential_12/lstm_6/strided_slice/stack:output:03sequential_12/lstm_6/strided_slice/stack_1:output:03sequential_12/lstm_6/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2$
"sequential_12/lstm_6/strided_slice?
 sequential_12/lstm_6/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_12/lstm_6/zeros/mul/y?
sequential_12/lstm_6/zeros/mulMul+sequential_12/lstm_6/strided_slice:output:0)sequential_12/lstm_6/zeros/mul/y:output:0*
T0*
_output_shapes
: 2 
sequential_12/lstm_6/zeros/mul?
!sequential_12/lstm_6/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2#
!sequential_12/lstm_6/zeros/Less/y?
sequential_12/lstm_6/zeros/LessLess"sequential_12/lstm_6/zeros/mul:z:0*sequential_12/lstm_6/zeros/Less/y:output:0*
T0*
_output_shapes
: 2!
sequential_12/lstm_6/zeros/Less?
#sequential_12/lstm_6/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2%
#sequential_12/lstm_6/zeros/packed/1?
!sequential_12/lstm_6/zeros/packedPack+sequential_12/lstm_6/strided_slice:output:0,sequential_12/lstm_6/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2#
!sequential_12/lstm_6/zeros/packed?
 sequential_12/lstm_6/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2"
 sequential_12/lstm_6/zeros/Const?
sequential_12/lstm_6/zerosFill*sequential_12/lstm_6/zeros/packed:output:0)sequential_12/lstm_6/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
sequential_12/lstm_6/zeros?
"sequential_12/lstm_6/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_12/lstm_6/zeros_1/mul/y?
 sequential_12/lstm_6/zeros_1/mulMul+sequential_12/lstm_6/strided_slice:output:0+sequential_12/lstm_6/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2"
 sequential_12/lstm_6/zeros_1/mul?
#sequential_12/lstm_6/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2%
#sequential_12/lstm_6/zeros_1/Less/y?
!sequential_12/lstm_6/zeros_1/LessLess$sequential_12/lstm_6/zeros_1/mul:z:0,sequential_12/lstm_6/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2#
!sequential_12/lstm_6/zeros_1/Less?
%sequential_12/lstm_6/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%sequential_12/lstm_6/zeros_1/packed/1?
#sequential_12/lstm_6/zeros_1/packedPack+sequential_12/lstm_6/strided_slice:output:0.sequential_12/lstm_6/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2%
#sequential_12/lstm_6/zeros_1/packed?
"sequential_12/lstm_6/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"sequential_12/lstm_6/zeros_1/Const?
sequential_12/lstm_6/zeros_1Fill,sequential_12/lstm_6/zeros_1/packed:output:0+sequential_12/lstm_6/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
sequential_12/lstm_6/zeros_1?
#sequential_12/lstm_6/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2%
#sequential_12/lstm_6/transpose/perm?
sequential_12/lstm_6/transpose	Transposelstm_6_input,sequential_12/lstm_6/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2 
sequential_12/lstm_6/transpose?
sequential_12/lstm_6/Shape_1Shape"sequential_12/lstm_6/transpose:y:0*
T0*
_output_shapes
:2
sequential_12/lstm_6/Shape_1?
*sequential_12/lstm_6/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_12/lstm_6/strided_slice_1/stack?
,sequential_12/lstm_6/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_6/strided_slice_1/stack_1?
,sequential_12/lstm_6/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_6/strided_slice_1/stack_2?
$sequential_12/lstm_6/strided_slice_1StridedSlice%sequential_12/lstm_6/Shape_1:output:03sequential_12/lstm_6/strided_slice_1/stack:output:05sequential_12/lstm_6/strided_slice_1/stack_1:output:05sequential_12/lstm_6/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2&
$sequential_12/lstm_6/strided_slice_1?
0sequential_12/lstm_6/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????22
0sequential_12/lstm_6/TensorArrayV2/element_shape?
"sequential_12/lstm_6/TensorArrayV2TensorListReserve9sequential_12/lstm_6/TensorArrayV2/element_shape:output:0-sequential_12/lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02$
"sequential_12/lstm_6/TensorArrayV2?
Jsequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2L
Jsequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape?
<sequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor"sequential_12/lstm_6/transpose:y:0Ssequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02>
<sequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensor?
*sequential_12/lstm_6/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_12/lstm_6/strided_slice_2/stack?
,sequential_12/lstm_6/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_6/strided_slice_2/stack_1?
,sequential_12/lstm_6/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_6/strided_slice_2/stack_2?
$sequential_12/lstm_6/strided_slice_2StridedSlice"sequential_12/lstm_6/transpose:y:03sequential_12/lstm_6/strided_slice_2/stack:output:05sequential_12/lstm_6/strided_slice_2/stack_1:output:05sequential_12/lstm_6/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2&
$sequential_12/lstm_6/strided_slice_2?
7sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp@sequential_12_lstm_6_lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype029
7sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp?
(sequential_12/lstm_6/lstm_cell_17/MatMulMatMul-sequential_12/lstm_6/strided_slice_2:output:0?sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2*
(sequential_12/lstm_6/lstm_cell_17/MatMul?
9sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOpBsequential_12_lstm_6_lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02;
9sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp?
*sequential_12/lstm_6/lstm_cell_17/MatMul_1MatMul#sequential_12/lstm_6/zeros:output:0Asequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2,
*sequential_12/lstm_6/lstm_cell_17/MatMul_1?
%sequential_12/lstm_6/lstm_cell_17/addAddV22sequential_12/lstm_6/lstm_cell_17/MatMul:product:04sequential_12/lstm_6/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2'
%sequential_12/lstm_6/lstm_cell_17/add?
8sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOpAsequential_12_lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02:
8sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp?
)sequential_12/lstm_6/lstm_cell_17/BiasAddBiasAdd)sequential_12/lstm_6/lstm_cell_17/add:z:0@sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2+
)sequential_12/lstm_6/lstm_cell_17/BiasAdd?
'sequential_12/lstm_6/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2)
'sequential_12/lstm_6/lstm_cell_17/Const?
1sequential_12/lstm_6/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :23
1sequential_12/lstm_6/lstm_cell_17/split/split_dim?
'sequential_12/lstm_6/lstm_cell_17/splitSplit:sequential_12/lstm_6/lstm_cell_17/split/split_dim:output:02sequential_12/lstm_6/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2)
'sequential_12/lstm_6/lstm_cell_17/split?
)sequential_12/lstm_6/lstm_cell_17/SigmoidSigmoid0sequential_12/lstm_6/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2+
)sequential_12/lstm_6/lstm_cell_17/Sigmoid?
+sequential_12/lstm_6/lstm_cell_17/Sigmoid_1Sigmoid0sequential_12/lstm_6/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_6/lstm_cell_17/Sigmoid_1?
%sequential_12/lstm_6/lstm_cell_17/mulMul/sequential_12/lstm_6/lstm_cell_17/Sigmoid_1:y:0%sequential_12/lstm_6/zeros_1:output:0*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_6/lstm_cell_17/mul?
&sequential_12/lstm_6/lstm_cell_17/ReluRelu0sequential_12/lstm_6/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2(
&sequential_12/lstm_6/lstm_cell_17/Relu?
'sequential_12/lstm_6/lstm_cell_17/mul_1Mul-sequential_12/lstm_6/lstm_cell_17/Sigmoid:y:04sequential_12/lstm_6/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_6/lstm_cell_17/mul_1?
'sequential_12/lstm_6/lstm_cell_17/add_1AddV2)sequential_12/lstm_6/lstm_cell_17/mul:z:0+sequential_12/lstm_6/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_6/lstm_cell_17/add_1?
+sequential_12/lstm_6/lstm_cell_17/Sigmoid_2Sigmoid0sequential_12/lstm_6/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_6/lstm_cell_17/Sigmoid_2?
(sequential_12/lstm_6/lstm_cell_17/Relu_1Relu+sequential_12/lstm_6/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2*
(sequential_12/lstm_6/lstm_cell_17/Relu_1?
'sequential_12/lstm_6/lstm_cell_17/mul_2Mul/sequential_12/lstm_6/lstm_cell_17/Sigmoid_2:y:06sequential_12/lstm_6/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_6/lstm_cell_17/mul_2?
2sequential_12/lstm_6/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   24
2sequential_12/lstm_6/TensorArrayV2_1/element_shape?
$sequential_12/lstm_6/TensorArrayV2_1TensorListReserve;sequential_12/lstm_6/TensorArrayV2_1/element_shape:output:0-sequential_12/lstm_6/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02&
$sequential_12/lstm_6/TensorArrayV2_1x
sequential_12/lstm_6/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_12/lstm_6/time?
-sequential_12/lstm_6/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2/
-sequential_12/lstm_6/while/maximum_iterations?
'sequential_12/lstm_6/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2)
'sequential_12/lstm_6/while/loop_counter?
sequential_12/lstm_6/whileWhile0sequential_12/lstm_6/while/loop_counter:output:06sequential_12/lstm_6/while/maximum_iterations:output:0"sequential_12/lstm_6/time:output:0-sequential_12/lstm_6/TensorArrayV2_1:handle:0#sequential_12/lstm_6/zeros:output:0%sequential_12/lstm_6/zeros_1:output:0-sequential_12/lstm_6/strided_slice_1:output:0Lsequential_12/lstm_6/TensorArrayUnstack/TensorListFromTensor:output_handle:0@sequential_12_lstm_6_lstm_cell_17_matmul_readvariableop_resourceBsequential_12_lstm_6_lstm_cell_17_matmul_1_readvariableop_resourceAsequential_12_lstm_6_lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*2
body*R(
&sequential_12_lstm_6_while_body_625858*2
cond*R(
&sequential_12_lstm_6_while_cond_625857*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
sequential_12/lstm_6/while?
Esequential_12/lstm_6/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2G
Esequential_12/lstm_6/TensorArrayV2Stack/TensorListStack/element_shape?
7sequential_12/lstm_6/TensorArrayV2Stack/TensorListStackTensorListStack#sequential_12/lstm_6/while:output:3Nsequential_12/lstm_6/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype029
7sequential_12/lstm_6/TensorArrayV2Stack/TensorListStack?
*sequential_12/lstm_6/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2,
*sequential_12/lstm_6/strided_slice_3/stack?
,sequential_12/lstm_6/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2.
,sequential_12/lstm_6/strided_slice_3/stack_1?
,sequential_12/lstm_6/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_6/strided_slice_3/stack_2?
$sequential_12/lstm_6/strided_slice_3StridedSlice@sequential_12/lstm_6/TensorArrayV2Stack/TensorListStack:tensor:03sequential_12/lstm_6/strided_slice_3/stack:output:05sequential_12/lstm_6/strided_slice_3/stack_1:output:05sequential_12/lstm_6/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2&
$sequential_12/lstm_6/strided_slice_3?
%sequential_12/lstm_6/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2'
%sequential_12/lstm_6/transpose_1/perm?
 sequential_12/lstm_6/transpose_1	Transpose@sequential_12/lstm_6/TensorArrayV2Stack/TensorListStack:tensor:0.sequential_12/lstm_6/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2"
 sequential_12/lstm_6/transpose_1?
sequential_12/lstm_6/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_12/lstm_6/runtime?
sequential_12/lstm_7/ShapeShape$sequential_12/lstm_6/transpose_1:y:0*
T0*
_output_shapes
:2
sequential_12/lstm_7/Shape?
(sequential_12/lstm_7/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2*
(sequential_12/lstm_7/strided_slice/stack?
*sequential_12/lstm_7/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_12/lstm_7/strided_slice/stack_1?
*sequential_12/lstm_7/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2,
*sequential_12/lstm_7/strided_slice/stack_2?
"sequential_12/lstm_7/strided_sliceStridedSlice#sequential_12/lstm_7/Shape:output:01sequential_12/lstm_7/strided_slice/stack:output:03sequential_12/lstm_7/strided_slice/stack_1:output:03sequential_12/lstm_7/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2$
"sequential_12/lstm_7/strided_slice?
 sequential_12/lstm_7/zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2"
 sequential_12/lstm_7/zeros/mul/y?
sequential_12/lstm_7/zeros/mulMul+sequential_12/lstm_7/strided_slice:output:0)sequential_12/lstm_7/zeros/mul/y:output:0*
T0*
_output_shapes
: 2 
sequential_12/lstm_7/zeros/mul?
!sequential_12/lstm_7/zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2#
!sequential_12/lstm_7/zeros/Less/y?
sequential_12/lstm_7/zeros/LessLess"sequential_12/lstm_7/zeros/mul:z:0*sequential_12/lstm_7/zeros/Less/y:output:0*
T0*
_output_shapes
: 2!
sequential_12/lstm_7/zeros/Less?
#sequential_12/lstm_7/zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2%
#sequential_12/lstm_7/zeros/packed/1?
!sequential_12/lstm_7/zeros/packedPack+sequential_12/lstm_7/strided_slice:output:0,sequential_12/lstm_7/zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2#
!sequential_12/lstm_7/zeros/packed?
 sequential_12/lstm_7/zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2"
 sequential_12/lstm_7/zeros/Const?
sequential_12/lstm_7/zerosFill*sequential_12/lstm_7/zeros/packed:output:0)sequential_12/lstm_7/zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
sequential_12/lstm_7/zeros?
"sequential_12/lstm_7/zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2$
"sequential_12/lstm_7/zeros_1/mul/y?
 sequential_12/lstm_7/zeros_1/mulMul+sequential_12/lstm_7/strided_slice:output:0+sequential_12/lstm_7/zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2"
 sequential_12/lstm_7/zeros_1/mul?
#sequential_12/lstm_7/zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2%
#sequential_12/lstm_7/zeros_1/Less/y?
!sequential_12/lstm_7/zeros_1/LessLess$sequential_12/lstm_7/zeros_1/mul:z:0,sequential_12/lstm_7/zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2#
!sequential_12/lstm_7/zeros_1/Less?
%sequential_12/lstm_7/zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2'
%sequential_12/lstm_7/zeros_1/packed/1?
#sequential_12/lstm_7/zeros_1/packedPack+sequential_12/lstm_7/strided_slice:output:0.sequential_12/lstm_7/zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2%
#sequential_12/lstm_7/zeros_1/packed?
"sequential_12/lstm_7/zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2$
"sequential_12/lstm_7/zeros_1/Const?
sequential_12/lstm_7/zeros_1Fill,sequential_12/lstm_7/zeros_1/packed:output:0+sequential_12/lstm_7/zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2
sequential_12/lstm_7/zeros_1?
#sequential_12/lstm_7/transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2%
#sequential_12/lstm_7/transpose/perm?
sequential_12/lstm_7/transpose	Transpose$sequential_12/lstm_6/transpose_1:y:0,sequential_12/lstm_7/transpose/perm:output:0*
T0*+
_output_shapes
:x?????????2 
sequential_12/lstm_7/transpose?
sequential_12/lstm_7/Shape_1Shape"sequential_12/lstm_7/transpose:y:0*
T0*
_output_shapes
:2
sequential_12/lstm_7/Shape_1?
*sequential_12/lstm_7/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_12/lstm_7/strided_slice_1/stack?
,sequential_12/lstm_7/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_7/strided_slice_1/stack_1?
,sequential_12/lstm_7/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_7/strided_slice_1/stack_2?
$sequential_12/lstm_7/strided_slice_1StridedSlice%sequential_12/lstm_7/Shape_1:output:03sequential_12/lstm_7/strided_slice_1/stack:output:05sequential_12/lstm_7/strided_slice_1/stack_1:output:05sequential_12/lstm_7/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2&
$sequential_12/lstm_7/strided_slice_1?
0sequential_12/lstm_7/TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????22
0sequential_12/lstm_7/TensorArrayV2/element_shape?
"sequential_12/lstm_7/TensorArrayV2TensorListReserve9sequential_12/lstm_7/TensorArrayV2/element_shape:output:0-sequential_12/lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02$
"sequential_12/lstm_7/TensorArrayV2?
Jsequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2L
Jsequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape?
<sequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensorTensorListFromTensor"sequential_12/lstm_7/transpose:y:0Ssequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02>
<sequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensor?
*sequential_12/lstm_7/strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2,
*sequential_12/lstm_7/strided_slice_2/stack?
,sequential_12/lstm_7/strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_7/strided_slice_2/stack_1?
,sequential_12/lstm_7/strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_7/strided_slice_2/stack_2?
$sequential_12/lstm_7/strided_slice_2StridedSlice"sequential_12/lstm_7/transpose:y:03sequential_12/lstm_7/strided_slice_2/stack:output:05sequential_12/lstm_7/strided_slice_2/stack_1:output:05sequential_12/lstm_7/strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2&
$sequential_12/lstm_7/strided_slice_2?
7sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp@sequential_12_lstm_7_lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype029
7sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp?
(sequential_12/lstm_7/lstm_cell_18/MatMulMatMul-sequential_12/lstm_7/strided_slice_2:output:0?sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2*
(sequential_12/lstm_7/lstm_cell_18/MatMul?
9sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOpBsequential_12_lstm_7_lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02;
9sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp?
*sequential_12/lstm_7/lstm_cell_18/MatMul_1MatMul#sequential_12/lstm_7/zeros:output:0Asequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2,
*sequential_12/lstm_7/lstm_cell_18/MatMul_1?
%sequential_12/lstm_7/lstm_cell_18/addAddV22sequential_12/lstm_7/lstm_cell_18/MatMul:product:04sequential_12/lstm_7/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2'
%sequential_12/lstm_7/lstm_cell_18/add?
8sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOpAsequential_12_lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02:
8sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp?
)sequential_12/lstm_7/lstm_cell_18/BiasAddBiasAdd)sequential_12/lstm_7/lstm_cell_18/add:z:0@sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2+
)sequential_12/lstm_7/lstm_cell_18/BiasAdd?
'sequential_12/lstm_7/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2)
'sequential_12/lstm_7/lstm_cell_18/Const?
1sequential_12/lstm_7/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :23
1sequential_12/lstm_7/lstm_cell_18/split/split_dim?
'sequential_12/lstm_7/lstm_cell_18/splitSplit:sequential_12/lstm_7/lstm_cell_18/split/split_dim:output:02sequential_12/lstm_7/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2)
'sequential_12/lstm_7/lstm_cell_18/split?
)sequential_12/lstm_7/lstm_cell_18/SigmoidSigmoid0sequential_12/lstm_7/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2+
)sequential_12/lstm_7/lstm_cell_18/Sigmoid?
+sequential_12/lstm_7/lstm_cell_18/Sigmoid_1Sigmoid0sequential_12/lstm_7/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_7/lstm_cell_18/Sigmoid_1?
%sequential_12/lstm_7/lstm_cell_18/mulMul/sequential_12/lstm_7/lstm_cell_18/Sigmoid_1:y:0%sequential_12/lstm_7/zeros_1:output:0*
T0*'
_output_shapes
:?????????2'
%sequential_12/lstm_7/lstm_cell_18/mul?
&sequential_12/lstm_7/lstm_cell_18/ReluRelu0sequential_12/lstm_7/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2(
&sequential_12/lstm_7/lstm_cell_18/Relu?
'sequential_12/lstm_7/lstm_cell_18/mul_1Mul-sequential_12/lstm_7/lstm_cell_18/Sigmoid:y:04sequential_12/lstm_7/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_7/lstm_cell_18/mul_1?
'sequential_12/lstm_7/lstm_cell_18/add_1AddV2)sequential_12/lstm_7/lstm_cell_18/mul:z:0+sequential_12/lstm_7/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_7/lstm_cell_18/add_1?
+sequential_12/lstm_7/lstm_cell_18/Sigmoid_2Sigmoid0sequential_12/lstm_7/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2-
+sequential_12/lstm_7/lstm_cell_18/Sigmoid_2?
(sequential_12/lstm_7/lstm_cell_18/Relu_1Relu+sequential_12/lstm_7/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2*
(sequential_12/lstm_7/lstm_cell_18/Relu_1?
'sequential_12/lstm_7/lstm_cell_18/mul_2Mul/sequential_12/lstm_7/lstm_cell_18/Sigmoid_2:y:06sequential_12/lstm_7/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2)
'sequential_12/lstm_7/lstm_cell_18/mul_2?
2sequential_12/lstm_7/TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   24
2sequential_12/lstm_7/TensorArrayV2_1/element_shape?
$sequential_12/lstm_7/TensorArrayV2_1TensorListReserve;sequential_12/lstm_7/TensorArrayV2_1/element_shape:output:0-sequential_12/lstm_7/strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02&
$sequential_12/lstm_7/TensorArrayV2_1x
sequential_12/lstm_7/timeConst*
_output_shapes
: *
dtype0*
value	B : 2
sequential_12/lstm_7/time?
-sequential_12/lstm_7/while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2/
-sequential_12/lstm_7/while/maximum_iterations?
'sequential_12/lstm_7/while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2)
'sequential_12/lstm_7/while/loop_counter?
sequential_12/lstm_7/whileWhile0sequential_12/lstm_7/while/loop_counter:output:06sequential_12/lstm_7/while/maximum_iterations:output:0"sequential_12/lstm_7/time:output:0-sequential_12/lstm_7/TensorArrayV2_1:handle:0#sequential_12/lstm_7/zeros:output:0%sequential_12/lstm_7/zeros_1:output:0-sequential_12/lstm_7/strided_slice_1:output:0Lsequential_12/lstm_7/TensorArrayUnstack/TensorListFromTensor:output_handle:0@sequential_12_lstm_7_lstm_cell_18_matmul_readvariableop_resourceBsequential_12_lstm_7_lstm_cell_18_matmul_1_readvariableop_resourceAsequential_12_lstm_7_lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*2
body*R(
&sequential_12_lstm_7_while_body_626007*2
cond*R(
&sequential_12_lstm_7_while_cond_626006*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
sequential_12/lstm_7/while?
Esequential_12/lstm_7/TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2G
Esequential_12/lstm_7/TensorArrayV2Stack/TensorListStack/element_shape?
7sequential_12/lstm_7/TensorArrayV2Stack/TensorListStackTensorListStack#sequential_12/lstm_7/while:output:3Nsequential_12/lstm_7/TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype029
7sequential_12/lstm_7/TensorArrayV2Stack/TensorListStack?
*sequential_12/lstm_7/strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2,
*sequential_12/lstm_7/strided_slice_3/stack?
,sequential_12/lstm_7/strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2.
,sequential_12/lstm_7/strided_slice_3/stack_1?
,sequential_12/lstm_7/strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2.
,sequential_12/lstm_7/strided_slice_3/stack_2?
$sequential_12/lstm_7/strided_slice_3StridedSlice@sequential_12/lstm_7/TensorArrayV2Stack/TensorListStack:tensor:03sequential_12/lstm_7/strided_slice_3/stack:output:05sequential_12/lstm_7/strided_slice_3/stack_1:output:05sequential_12/lstm_7/strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2&
$sequential_12/lstm_7/strided_slice_3?
%sequential_12/lstm_7/transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2'
%sequential_12/lstm_7/transpose_1/perm?
 sequential_12/lstm_7/transpose_1	Transpose@sequential_12/lstm_7/TensorArrayV2Stack/TensorListStack:tensor:0.sequential_12/lstm_7/transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2"
 sequential_12/lstm_7/transpose_1?
sequential_12/lstm_7/runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2
sequential_12/lstm_7/runtime?
,sequential_12/dense_18/MatMul/ReadVariableOpReadVariableOp5sequential_12_dense_18_matmul_readvariableop_resource*
_output_shapes

:*
dtype02.
,sequential_12/dense_18/MatMul/ReadVariableOp?
sequential_12/dense_18/MatMulMatMul-sequential_12/lstm_7/strided_slice_3:output:04sequential_12/dense_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
sequential_12/dense_18/MatMul?
-sequential_12/dense_18/BiasAdd/ReadVariableOpReadVariableOp6sequential_12_dense_18_biasadd_readvariableop_resource*
_output_shapes
:*
dtype02/
-sequential_12/dense_18/BiasAdd/ReadVariableOp?
sequential_12/dense_18/BiasAddBiasAdd'sequential_12/dense_18/MatMul:product:05sequential_12/dense_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2 
sequential_12/dense_18/BiasAdd?
IdentityIdentity'sequential_12/dense_18/BiasAdd:output:0.^sequential_12/dense_18/BiasAdd/ReadVariableOp-^sequential_12/dense_18/MatMul/ReadVariableOp9^sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp8^sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp:^sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp^sequential_12/lstm_6/while9^sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp8^sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp:^sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp^sequential_12/lstm_7/while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*J
_input_shapes9
7:?????????x::::::::2^
-sequential_12/dense_18/BiasAdd/ReadVariableOp-sequential_12/dense_18/BiasAdd/ReadVariableOp2\
,sequential_12/dense_18/MatMul/ReadVariableOp,sequential_12/dense_18/MatMul/ReadVariableOp2t
8sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp8sequential_12/lstm_6/lstm_cell_17/BiasAdd/ReadVariableOp2r
7sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp7sequential_12/lstm_6/lstm_cell_17/MatMul/ReadVariableOp2v
9sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp9sequential_12/lstm_6/lstm_cell_17/MatMul_1/ReadVariableOp28
sequential_12/lstm_6/whilesequential_12/lstm_6/while2t
8sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp8sequential_12/lstm_7/lstm_cell_18/BiasAdd/ReadVariableOp2r
7sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp7sequential_12/lstm_7/lstm_cell_18/MatMul/ReadVariableOp2v
9sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp9sequential_12/lstm_7/lstm_cell_18/MatMul_1/ReadVariableOp28
sequential_12/lstm_7/whilesequential_12/lstm_7/while:Y U
+
_output_shapes
:?????????x
&
_user_specified_namelstm_6_input
?	
?
D__inference_dense_18_layer_call_and_return_conditional_losses_628002

inputs"
matmul_readvariableop_resource#
biasadd_readvariableop_resource
identity??BiasAdd/ReadVariableOp?MatMul/ReadVariableOp?
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:*
dtype02
MatMul/ReadVariableOps
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2
MatMul?
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype02
BiasAdd/ReadVariableOp?
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????2	
BiasAdd?
IdentityIdentityBiasAdd:output:0^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*.
_input_shapes
:?????????::20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs
?
?
while_cond_627723
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627723___redundant_placeholder04
0while_while_cond_627723___redundant_placeholder14
0while_while_cond_627723___redundant_placeholder24
0while_while_cond_627723___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?M
?	
lstm_6_while_body_628230*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3)
%lstm_6_while_lstm_6_strided_slice_1_0e
alstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0>
:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0@
<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0?
;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0
lstm_6_while_identity
lstm_6_while_identity_1
lstm_6_while_identity_2
lstm_6_while_identity_3
lstm_6_while_identity_4
lstm_6_while_identity_5'
#lstm_6_while_lstm_6_strided_slice_1c
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor<
8lstm_6_while_lstm_cell_17_matmul_readvariableop_resource>
:lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource=
9lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource??0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp?1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2@
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0lstm_6_while_placeholderGlstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype022
0lstm_6/while/TensorArrayV2Read/TensorListGetItem?
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype021
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp?
 lstm_6/while/lstm_cell_17/MatMulMatMul7lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:07lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2"
 lstm_6/while/lstm_cell_17/MatMul?
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype023
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
"lstm_6/while/lstm_cell_17/MatMul_1MatMullstm_6_while_placeholder_29lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2$
"lstm_6/while/lstm_cell_17/MatMul_1?
lstm_6/while/lstm_cell_17/addAddV2*lstm_6/while/lstm_cell_17/MatMul:product:0,lstm_6/while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_6/while/lstm_cell_17/add?
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype022
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?
!lstm_6/while/lstm_cell_17/BiasAddBiasAdd!lstm_6/while/lstm_cell_17/add:z:08lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2#
!lstm_6/while/lstm_cell_17/BiasAdd?
lstm_6/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2!
lstm_6/while/lstm_cell_17/Const?
)lstm_6/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2+
)lstm_6/while/lstm_cell_17/split/split_dim?
lstm_6/while/lstm_cell_17/splitSplit2lstm_6/while/lstm_cell_17/split/split_dim:output:0*lstm_6/while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2!
lstm_6/while/lstm_cell_17/split?
!lstm_6/while/lstm_cell_17/SigmoidSigmoid(lstm_6/while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2#
!lstm_6/while/lstm_cell_17/Sigmoid?
#lstm_6/while/lstm_cell_17/Sigmoid_1Sigmoid(lstm_6/while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2%
#lstm_6/while/lstm_cell_17/Sigmoid_1?
lstm_6/while/lstm_cell_17/mulMul'lstm_6/while/lstm_cell_17/Sigmoid_1:y:0lstm_6_while_placeholder_3*
T0*'
_output_shapes
:?????????2
lstm_6/while/lstm_cell_17/mul?
lstm_6/while/lstm_cell_17/ReluRelu(lstm_6/while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2 
lstm_6/while/lstm_cell_17/Relu?
lstm_6/while/lstm_cell_17/mul_1Mul%lstm_6/while/lstm_cell_17/Sigmoid:y:0,lstm_6/while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/mul_1?
lstm_6/while/lstm_cell_17/add_1AddV2!lstm_6/while/lstm_cell_17/mul:z:0#lstm_6/while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/add_1?
#lstm_6/while/lstm_cell_17/Sigmoid_2Sigmoid(lstm_6/while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2%
#lstm_6/while/lstm_cell_17/Sigmoid_2?
 lstm_6/while/lstm_cell_17/Relu_1Relu#lstm_6/while/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2"
 lstm_6/while/lstm_cell_17/Relu_1?
lstm_6/while/lstm_cell_17/mul_2Mul'lstm_6/while/lstm_cell_17/Sigmoid_2:y:0.lstm_6/while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/mul_2?
1lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_6_while_placeholder_1lstm_6_while_placeholder#lstm_6/while/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype023
1lstm_6/while/TensorArrayV2Write/TensorListSetItemj
lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add/y?
lstm_6/while/addAddV2lstm_6_while_placeholderlstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/addn
lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add_1/y?
lstm_6/while/add_1AddV2&lstm_6_while_lstm_6_while_loop_counterlstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/add_1?
lstm_6/while/IdentityIdentitylstm_6/while/add_1:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity?
lstm_6/while/Identity_1Identity,lstm_6_while_lstm_6_while_maximum_iterations1^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_1?
lstm_6/while/Identity_2Identitylstm_6/while/add:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_2?
lstm_6/while/Identity_3IdentityAlstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_3?
lstm_6/while/Identity_4Identity#lstm_6/while/lstm_cell_17/mul_2:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_6/while/Identity_4?
lstm_6/while/Identity_5Identity#lstm_6/while/lstm_cell_17/add_1:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_6/while/Identity_5"7
lstm_6_while_identitylstm_6/while/Identity:output:0";
lstm_6_while_identity_1 lstm_6/while/Identity_1:output:0";
lstm_6_while_identity_2 lstm_6/while/Identity_2:output:0";
lstm_6_while_identity_3 lstm_6/while/Identity_3:output:0";
lstm_6_while_identity_4 lstm_6/while/Identity_4:output:0";
lstm_6_while_identity_5 lstm_6/while/Identity_5:output:0"L
#lstm_6_while_lstm_6_strided_slice_1%lstm_6_while_lstm_6_strided_slice_1_0"x
9lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0"z
:lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0"v
8lstm_6_while_lstm_cell_17_matmul_readvariableop_resource:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0"?
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensoralstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2d
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp2b
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2f
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_627627

inputs/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627542*
condR
while_cond_627541*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?M
?	
lstm_6_while_body_628538*
&lstm_6_while_lstm_6_while_loop_counter0
,lstm_6_while_lstm_6_while_maximum_iterations
lstm_6_while_placeholder
lstm_6_while_placeholder_1
lstm_6_while_placeholder_2
lstm_6_while_placeholder_3)
%lstm_6_while_lstm_6_strided_slice_1_0e
alstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0>
:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0@
<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0?
;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0
lstm_6_while_identity
lstm_6_while_identity_1
lstm_6_while_identity_2
lstm_6_while_identity_3
lstm_6_while_identity_4
lstm_6_while_identity_5'
#lstm_6_while_lstm_6_strided_slice_1c
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor<
8lstm_6_while_lstm_cell_17_matmul_readvariableop_resource>
:lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource=
9lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource??0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp?1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2@
>lstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape?
0lstm_6/while/TensorArrayV2Read/TensorListGetItemTensorListGetItemalstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0lstm_6_while_placeholderGlstm_6/while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype022
0lstm_6/while/TensorArrayV2Read/TensorListGetItem?
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype021
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp?
 lstm_6/while/lstm_cell_17/MatMulMatMul7lstm_6/while/TensorArrayV2Read/TensorListGetItem:item:07lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2"
 lstm_6/while/lstm_cell_17/MatMul?
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype023
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp?
"lstm_6/while/lstm_cell_17/MatMul_1MatMullstm_6_while_placeholder_29lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2$
"lstm_6/while/lstm_cell_17/MatMul_1?
lstm_6/while/lstm_cell_17/addAddV2*lstm_6/while/lstm_cell_17/MatMul:product:0,lstm_6/while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_6/while/lstm_cell_17/add?
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype022
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp?
!lstm_6/while/lstm_cell_17/BiasAddBiasAdd!lstm_6/while/lstm_cell_17/add:z:08lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2#
!lstm_6/while/lstm_cell_17/BiasAdd?
lstm_6/while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2!
lstm_6/while/lstm_cell_17/Const?
)lstm_6/while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2+
)lstm_6/while/lstm_cell_17/split/split_dim?
lstm_6/while/lstm_cell_17/splitSplit2lstm_6/while/lstm_cell_17/split/split_dim:output:0*lstm_6/while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2!
lstm_6/while/lstm_cell_17/split?
!lstm_6/while/lstm_cell_17/SigmoidSigmoid(lstm_6/while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2#
!lstm_6/while/lstm_cell_17/Sigmoid?
#lstm_6/while/lstm_cell_17/Sigmoid_1Sigmoid(lstm_6/while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2%
#lstm_6/while/lstm_cell_17/Sigmoid_1?
lstm_6/while/lstm_cell_17/mulMul'lstm_6/while/lstm_cell_17/Sigmoid_1:y:0lstm_6_while_placeholder_3*
T0*'
_output_shapes
:?????????2
lstm_6/while/lstm_cell_17/mul?
lstm_6/while/lstm_cell_17/ReluRelu(lstm_6/while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2 
lstm_6/while/lstm_cell_17/Relu?
lstm_6/while/lstm_cell_17/mul_1Mul%lstm_6/while/lstm_cell_17/Sigmoid:y:0,lstm_6/while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/mul_1?
lstm_6/while/lstm_cell_17/add_1AddV2!lstm_6/while/lstm_cell_17/mul:z:0#lstm_6/while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/add_1?
#lstm_6/while/lstm_cell_17/Sigmoid_2Sigmoid(lstm_6/while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2%
#lstm_6/while/lstm_cell_17/Sigmoid_2?
 lstm_6/while/lstm_cell_17/Relu_1Relu#lstm_6/while/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2"
 lstm_6/while/lstm_cell_17/Relu_1?
lstm_6/while/lstm_cell_17/mul_2Mul'lstm_6/while/lstm_cell_17/Sigmoid_2:y:0.lstm_6/while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2!
lstm_6/while/lstm_cell_17/mul_2?
1lstm_6/while/TensorArrayV2Write/TensorListSetItemTensorListSetItemlstm_6_while_placeholder_1lstm_6_while_placeholder#lstm_6/while/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype023
1lstm_6/while/TensorArrayV2Write/TensorListSetItemj
lstm_6/while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add/y?
lstm_6/while/addAddV2lstm_6_while_placeholderlstm_6/while/add/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/addn
lstm_6/while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_6/while/add_1/y?
lstm_6/while/add_1AddV2&lstm_6_while_lstm_6_while_loop_counterlstm_6/while/add_1/y:output:0*
T0*
_output_shapes
: 2
lstm_6/while/add_1?
lstm_6/while/IdentityIdentitylstm_6/while/add_1:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity?
lstm_6/while/Identity_1Identity,lstm_6_while_lstm_6_while_maximum_iterations1^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_1?
lstm_6/while/Identity_2Identitylstm_6/while/add:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_2?
lstm_6/while/Identity_3IdentityAlstm_6/while/TensorArrayV2Write/TensorListSetItem:output_handle:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
lstm_6/while/Identity_3?
lstm_6/while/Identity_4Identity#lstm_6/while/lstm_cell_17/mul_2:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_6/while/Identity_4?
lstm_6/while/Identity_5Identity#lstm_6/while/lstm_cell_17/add_1:z:01^lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0^lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2^lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
lstm_6/while/Identity_5"7
lstm_6_while_identitylstm_6/while/Identity:output:0";
lstm_6_while_identity_1 lstm_6/while/Identity_1:output:0";
lstm_6_while_identity_2 lstm_6/while/Identity_2:output:0";
lstm_6_while_identity_3 lstm_6/while/Identity_3:output:0";
lstm_6_while_identity_4 lstm_6/while/Identity_4:output:0";
lstm_6_while_identity_5 lstm_6/while/Identity_5:output:0"L
#lstm_6_while_lstm_6_strided_slice_1%lstm_6_while_lstm_6_strided_slice_1_0"x
9lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource;lstm_6_while_lstm_cell_17_biasadd_readvariableop_resource_0"z
:lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource<lstm_6_while_lstm_cell_17_matmul_1_readvariableop_resource_0"v
8lstm_6_while_lstm_cell_17_matmul_readvariableop_resource:lstm_6_while_lstm_cell_17_matmul_readvariableop_resource_0"?
_lstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensoralstm_6_while_tensorarrayv2read_tensorlistgetitem_lstm_6_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2d
0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp0lstm_6/while/lstm_cell_17/BiasAdd/ReadVariableOp2b
/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp/lstm_6/while/lstm_cell_17/MatMul/ReadVariableOp2f
1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp1lstm_6/while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
-__inference_lstm_cell_17_layer_call_fn_630234

inputs
states_0
states_1
unknown
	unknown_0
	unknown_1
identity

identity_1

identity_2??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsstates_0states_1unknown	unknown_0	unknown_1*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_6261712
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity?

Identity_1Identity StatefulPartitionedCall:output:1^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_1?

Identity_2Identity StatefulPartitionedCall:output:2^StatefulPartitionedCall*
T0*'
_output_shapes
:?????????2

Identity_2"
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0*X
_input_shapesG
E:?????????:?????????:?????????:::22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:?????????
 
_user_specified_nameinputs:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/0:QM
'
_output_shapes
:?????????
"
_user_specified_name
states/1
?[
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_627809

inputs/
+lstm_cell_18_matmul_readvariableop_resource1
-lstm_cell_18_matmul_1_readvariableop_resource0
,lstm_cell_18_biasadd_readvariableop_resource
identity??#lstm_cell_18/BiasAdd/ReadVariableOp?"lstm_cell_18/MatMul/ReadVariableOp?$lstm_cell_18/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_18/MatMul/ReadVariableOpReadVariableOp+lstm_cell_18_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_18/MatMul/ReadVariableOp?
lstm_cell_18/MatMulMatMulstrided_slice_2:output:0*lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul?
$lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_18_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_18/MatMul_1/ReadVariableOp?
lstm_cell_18/MatMul_1MatMulzeros:output:0,lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/MatMul_1?
lstm_cell_18/addAddV2lstm_cell_18/MatMul:product:0lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/add?
#lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_18_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_18/BiasAdd/ReadVariableOp?
lstm_cell_18/BiasAddBiasAddlstm_cell_18/add:z:0+lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_18/BiasAddj
lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/Const~
lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_18/split/split_dim?
lstm_cell_18/splitSplit%lstm_cell_18/split/split_dim:output:0lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_18/split?
lstm_cell_18/SigmoidSigmoidlstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid?
lstm_cell_18/Sigmoid_1Sigmoidlstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_1?
lstm_cell_18/mulMullstm_cell_18/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul}
lstm_cell_18/ReluRelulstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu?
lstm_cell_18/mul_1Mullstm_cell_18/Sigmoid:y:0lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_1?
lstm_cell_18/add_1AddV2lstm_cell_18/mul:z:0lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/add_1?
lstm_cell_18/Sigmoid_2Sigmoidlstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Sigmoid_2|
lstm_cell_18/Relu_1Relulstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/Relu_1?
lstm_cell_18/mul_2Mullstm_cell_18/Sigmoid_2:y:0!lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_18/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_18_matmul_readvariableop_resource-lstm_cell_18_matmul_1_readvariableop_resource,lstm_cell_18_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627724*
condR
while_cond_627723*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0$^lstm_cell_18/BiasAdd/ReadVariableOp#^lstm_cell_18/MatMul/ReadVariableOp%^lstm_cell_18/MatMul_1/ReadVariableOp^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_18/BiasAdd/ReadVariableOp#lstm_cell_18/BiasAdd/ReadVariableOp2H
"lstm_cell_18/MatMul/ReadVariableOp"lstm_cell_18/MatMul/ReadVariableOp2L
$lstm_cell_18/MatMul_1/ReadVariableOp$lstm_cell_18/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?B
?
while_body_629041
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_17_matmul_readvariableop_resource_09
5while_lstm_cell_17_matmul_1_readvariableop_resource_08
4while_lstm_cell_17_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_17_matmul_readvariableop_resource7
3while_lstm_cell_17_matmul_1_readvariableop_resource6
2while_lstm_cell_17_biasadd_readvariableop_resource??)while/lstm_cell_17/BiasAdd/ReadVariableOp?(while/lstm_cell_17/MatMul/ReadVariableOp?*while/lstm_cell_17/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_17/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_17_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_17/MatMul/ReadVariableOp?
while/lstm_cell_17/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul?
*while/lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_17_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_17/MatMul_1/ReadVariableOp?
while/lstm_cell_17/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/MatMul_1?
while/lstm_cell_17/addAddV2#while/lstm_cell_17/MatMul:product:0%while/lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/add?
)while/lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_17_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_17/BiasAdd/ReadVariableOp?
while/lstm_cell_17/BiasAddBiasAddwhile/lstm_cell_17/add:z:01while/lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_17/BiasAddv
while/lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_17/Const?
"while/lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_17/split/split_dim?
while/lstm_cell_17/splitSplit+while/lstm_cell_17/split/split_dim:output:0#while/lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_17/split?
while/lstm_cell_17/SigmoidSigmoid!while/lstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid?
while/lstm_cell_17/Sigmoid_1Sigmoid!while/lstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_1?
while/lstm_cell_17/mulMul while/lstm_cell_17/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul?
while/lstm_cell_17/ReluRelu!while/lstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu?
while/lstm_cell_17/mul_1Mulwhile/lstm_cell_17/Sigmoid:y:0%while/lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_1?
while/lstm_cell_17/add_1AddV2while/lstm_cell_17/mul:z:0while/lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/add_1?
while/lstm_cell_17/Sigmoid_2Sigmoid!while/lstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Sigmoid_2?
while/lstm_cell_17/Relu_1Reluwhile/lstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/Relu_1?
while/lstm_cell_17/mul_2Mul while/lstm_cell_17/Sigmoid_2:y:0'while/lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_17/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_17/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_17/mul_2:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_17/add_1:z:0*^while/lstm_cell_17/BiasAdd/ReadVariableOp)^while/lstm_cell_17/MatMul/ReadVariableOp+^while/lstm_cell_17/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_17_biasadd_readvariableop_resource4while_lstm_cell_17_biasadd_readvariableop_resource_0"l
3while_lstm_cell_17_matmul_1_readvariableop_resource5while_lstm_cell_17_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_17_matmul_readvariableop_resource3while_lstm_cell_17_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_17/BiasAdd/ReadVariableOp)while/lstm_cell_17/BiasAdd/ReadVariableOp2T
(while/lstm_cell_17/MatMul/ReadVariableOp(while/lstm_cell_17/MatMul/ReadVariableOp2X
*while/lstm_cell_17/MatMul_1/ReadVariableOp*while/lstm_cell_17/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629454

inputs/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/permz
	transpose	Transposeinputstranspose/perm:output:0*
T0*+
_output_shapes
:x?????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629369*
condR
while_cond_629368*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*+
_output_shapes
:x?????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*+
_output_shapes
:?????????x2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?B
?
while_body_629872
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_strided_slice_1_0W
Swhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_07
3while_lstm_cell_18_matmul_readvariableop_resource_09
5while_lstm_cell_18_matmul_1_readvariableop_resource_08
4while_lstm_cell_18_biasadd_readvariableop_resource_0
while_identity
while_identity_1
while_identity_2
while_identity_3
while_identity_4
while_identity_5
while_strided_slice_1U
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor5
1while_lstm_cell_18_matmul_readvariableop_resource7
3while_lstm_cell_18_matmul_1_readvariableop_resource6
2while_lstm_cell_18_biasadd_readvariableop_resource??)while/lstm_cell_18/BiasAdd/ReadVariableOp?(while/lstm_cell_18/MatMul/ReadVariableOp?*while/lstm_cell_18/MatMul_1/ReadVariableOp?
7while/TensorArrayV2Read/TensorListGetItem/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   29
7while/TensorArrayV2Read/TensorListGetItem/element_shape?
)while/TensorArrayV2Read/TensorListGetItemTensorListGetItemSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0while_placeholder@while/TensorArrayV2Read/TensorListGetItem/element_shape:output:0*'
_output_shapes
:?????????*
element_dtype02+
)while/TensorArrayV2Read/TensorListGetItem?
(while/lstm_cell_18/MatMul/ReadVariableOpReadVariableOp3while_lstm_cell_18_matmul_readvariableop_resource_0*
_output_shapes

:<*
dtype02*
(while/lstm_cell_18/MatMul/ReadVariableOp?
while/lstm_cell_18/MatMulMatMul0while/TensorArrayV2Read/TensorListGetItem:item:00while/lstm_cell_18/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul?
*while/lstm_cell_18/MatMul_1/ReadVariableOpReadVariableOp5while_lstm_cell_18_matmul_1_readvariableop_resource_0*
_output_shapes

:<*
dtype02,
*while/lstm_cell_18/MatMul_1/ReadVariableOp?
while/lstm_cell_18/MatMul_1MatMulwhile_placeholder_22while/lstm_cell_18/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/MatMul_1?
while/lstm_cell_18/addAddV2#while/lstm_cell_18/MatMul:product:0%while/lstm_cell_18/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/add?
)while/lstm_cell_18/BiasAdd/ReadVariableOpReadVariableOp4while_lstm_cell_18_biasadd_readvariableop_resource_0*
_output_shapes
:<*
dtype02+
)while/lstm_cell_18/BiasAdd/ReadVariableOp?
while/lstm_cell_18/BiasAddBiasAddwhile/lstm_cell_18/add:z:01while/lstm_cell_18/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
while/lstm_cell_18/BiasAddv
while/lstm_cell_18/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
while/lstm_cell_18/Const?
"while/lstm_cell_18/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2$
"while/lstm_cell_18/split/split_dim?
while/lstm_cell_18/splitSplit+while/lstm_cell_18/split/split_dim:output:0#while/lstm_cell_18/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
while/lstm_cell_18/split?
while/lstm_cell_18/SigmoidSigmoid!while/lstm_cell_18/split:output:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid?
while/lstm_cell_18/Sigmoid_1Sigmoid!while/lstm_cell_18/split:output:1*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_1?
while/lstm_cell_18/mulMul while/lstm_cell_18/Sigmoid_1:y:0while_placeholder_3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul?
while/lstm_cell_18/ReluRelu!while/lstm_cell_18/split:output:2*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu?
while/lstm_cell_18/mul_1Mulwhile/lstm_cell_18/Sigmoid:y:0%while/lstm_cell_18/Relu:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_1?
while/lstm_cell_18/add_1AddV2while/lstm_cell_18/mul:z:0while/lstm_cell_18/mul_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/add_1?
while/lstm_cell_18/Sigmoid_2Sigmoid!while/lstm_cell_18/split:output:3*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Sigmoid_2?
while/lstm_cell_18/Relu_1Reluwhile/lstm_cell_18/add_1:z:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/Relu_1?
while/lstm_cell_18/mul_2Mul while/lstm_cell_18/Sigmoid_2:y:0'while/lstm_cell_18/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
while/lstm_cell_18/mul_2?
*while/TensorArrayV2Write/TensorListSetItemTensorListSetItemwhile_placeholder_1while_placeholderwhile/lstm_cell_18/mul_2:z:0*
_output_shapes
: *
element_dtype02,
*while/TensorArrayV2Write/TensorListSetItem\
while/add/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add/yi
	while/addAddV2while_placeholderwhile/add/y:output:0*
T0*
_output_shapes
: 2
	while/add`
while/add_1/yConst*
_output_shapes
: *
dtype0*
value	B :2
while/add_1/yv
while/add_1AddV2while_while_loop_counterwhile/add_1/y:output:0*
T0*
_output_shapes
: 2
while/add_1?
while/IdentityIdentitywhile/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity?
while/Identity_1Identitywhile_while_maximum_iterations*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_1?
while/Identity_2Identitywhile/add:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_2?
while/Identity_3Identity:while/TensorArrayV2Write/TensorListSetItem:output_handle:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*
_output_shapes
: 2
while/Identity_3?
while/Identity_4Identitywhile/lstm_cell_18/mul_2:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_4?
while/Identity_5Identitywhile/lstm_cell_18/add_1:z:0*^while/lstm_cell_18/BiasAdd/ReadVariableOp)^while/lstm_cell_18/MatMul/ReadVariableOp+^while/lstm_cell_18/MatMul_1/ReadVariableOp*
T0*'
_output_shapes
:?????????2
while/Identity_5")
while_identitywhile/Identity:output:0"-
while_identity_1while/Identity_1:output:0"-
while_identity_2while/Identity_2:output:0"-
while_identity_3while/Identity_3:output:0"-
while_identity_4while/Identity_4:output:0"-
while_identity_5while/Identity_5:output:0"j
2while_lstm_cell_18_biasadd_readvariableop_resource4while_lstm_cell_18_biasadd_readvariableop_resource_0"l
3while_lstm_cell_18_matmul_1_readvariableop_resource5while_lstm_cell_18_matmul_1_readvariableop_resource_0"h
1while_lstm_cell_18_matmul_readvariableop_resource3while_lstm_cell_18_matmul_readvariableop_resource_0"0
while_strided_slice_1while_strided_slice_1_0"?
Qwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensorSwhile_tensorarrayv2read_tensorlistgetitem_tensorarrayunstack_tensorlistfromtensor_0*Q
_input_shapes@
>: : : : :?????????:?????????: : :::2V
)while/lstm_cell_18/BiasAdd/ReadVariableOp)while/lstm_cell_18/BiasAdd/ReadVariableOp2T
(while/lstm_cell_18/MatMul/ReadVariableOp(while/lstm_cell_18/MatMul/ReadVariableOp2X
*while/lstm_cell_18/MatMul_1/ReadVariableOp*while/lstm_cell_18/MatMul_1/ReadVariableOp: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
: 
?
?
'__inference_lstm_6_layer_call_fn_629465

inputs
unknown
	unknown_0
	unknown_1
identity??StatefulPartitionedCall?
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *+
_output_shapes
:?????????x*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *K
fFRD
B__inference_lstm_6_layer_call_and_return_conditional_losses_6274742
StatefulPartitionedCall?
IdentityIdentity StatefulPartitionedCall:output:0^StatefulPartitionedCall*
T0*+
_output_shapes
:?????????x2

Identity"
identityIdentity:output:0*6
_input_shapes%
#:?????????x:::22
StatefulPartitionedCallStatefulPartitionedCall:S O
+
_output_shapes
:?????????x
 
_user_specified_nameinputs
?[
?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629126
inputs_0/
+lstm_cell_17_matmul_readvariableop_resource1
-lstm_cell_17_matmul_1_readvariableop_resource0
,lstm_cell_17_biasadd_readvariableop_resource
identity??#lstm_cell_17/BiasAdd/ReadVariableOp?"lstm_cell_17/MatMul/ReadVariableOp?$lstm_cell_17/MatMul_1/ReadVariableOp?whileF
ShapeShapeinputs_0*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputs_0transpose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
"lstm_cell_17/MatMul/ReadVariableOpReadVariableOp+lstm_cell_17_matmul_readvariableop_resource*
_output_shapes

:<*
dtype02$
"lstm_cell_17/MatMul/ReadVariableOp?
lstm_cell_17/MatMulMatMulstrided_slice_2:output:0*lstm_cell_17/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul?
$lstm_cell_17/MatMul_1/ReadVariableOpReadVariableOp-lstm_cell_17_matmul_1_readvariableop_resource*
_output_shapes

:<*
dtype02&
$lstm_cell_17/MatMul_1/ReadVariableOp?
lstm_cell_17/MatMul_1MatMulzeros:output:0,lstm_cell_17/MatMul_1/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/MatMul_1?
lstm_cell_17/addAddV2lstm_cell_17/MatMul:product:0lstm_cell_17/MatMul_1:product:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/add?
#lstm_cell_17/BiasAdd/ReadVariableOpReadVariableOp,lstm_cell_17_biasadd_readvariableop_resource*
_output_shapes
:<*
dtype02%
#lstm_cell_17/BiasAdd/ReadVariableOp?
lstm_cell_17/BiasAddBiasAddlstm_cell_17/add:z:0+lstm_cell_17/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:?????????<2
lstm_cell_17/BiasAddj
lstm_cell_17/ConstConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/Const~
lstm_cell_17/split/split_dimConst*
_output_shapes
: *
dtype0*
value	B :2
lstm_cell_17/split/split_dim?
lstm_cell_17/splitSplit%lstm_cell_17/split/split_dim:output:0lstm_cell_17/BiasAdd:output:0*
T0*`
_output_shapesN
L:?????????:?????????:?????????:?????????*
	num_split2
lstm_cell_17/split?
lstm_cell_17/SigmoidSigmoidlstm_cell_17/split:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid?
lstm_cell_17/Sigmoid_1Sigmoidlstm_cell_17/split:output:1*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_1?
lstm_cell_17/mulMullstm_cell_17/Sigmoid_1:y:0zeros_1:output:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul}
lstm_cell_17/ReluRelulstm_cell_17/split:output:2*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu?
lstm_cell_17/mul_1Mullstm_cell_17/Sigmoid:y:0lstm_cell_17/Relu:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_1?
lstm_cell_17/add_1AddV2lstm_cell_17/mul:z:0lstm_cell_17/mul_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/add_1?
lstm_cell_17/Sigmoid_2Sigmoidlstm_cell_17/split:output:3*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Sigmoid_2|
lstm_cell_17/Relu_1Relulstm_cell_17/add_1:z:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/Relu_1?
lstm_cell_17/mul_2Mullstm_cell_17/Sigmoid_2:y:0!lstm_cell_17/Relu_1:activations:0*
T0*'
_output_shapes
:?????????2
lstm_cell_17/mul_2?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0+lstm_cell_17_matmul_readvariableop_resource-lstm_cell_17_matmul_1_readvariableop_resource,lstm_cell_17_biasadd_readvariableop_resource*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_629041*
condR
while_cond_629040*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitytranspose_1:y:0$^lstm_cell_17/BiasAdd/ReadVariableOp#^lstm_cell_17/MatMul/ReadVariableOp%^lstm_cell_17/MatMul_1/ReadVariableOp^while*
T0*4
_output_shapes"
 :??????????????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2J
#lstm_cell_17/BiasAdd/ReadVariableOp#lstm_cell_17/BiasAdd/ReadVariableOp2H
"lstm_cell_17/MatMul/ReadVariableOp"lstm_cell_17/MatMul/ReadVariableOp2L
$lstm_cell_17/MatMul_1/ReadVariableOp$lstm_cell_17/MatMul_1/ReadVariableOp2
whilewhile:^ Z
4
_output_shapes"
 :??????????????????
"
_user_specified_name
inputs/0
?
?
while_cond_627876
while_while_loop_counter"
while_while_maximum_iterations
while_placeholder
while_placeholder_1
while_placeholder_2
while_placeholder_3
while_less_strided_slice_14
0while_while_cond_627876___redundant_placeholder04
0while_while_cond_627876___redundant_placeholder14
0while_while_cond_627876___redundant_placeholder24
0while_while_cond_627876___redundant_placeholder3
while_identity
p

while/LessLesswhile_placeholderwhile_less_strided_slice_1*
T0*
_output_shapes
: 2

while/Less]
while/IdentityIdentitywhile/Less:z:0*
T0
*
_output_shapes
: 2
while/Identity")
while_identitywhile/Identity:output:0*S
_input_shapesB
@: : : : :?????????:?????????: ::::: 

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :-)
'
_output_shapes
:?????????:-)
'
_output_shapes
:?????????:

_output_shapes
: :

_output_shapes
:
?D
?
B__inference_lstm_7_layer_call_and_return_conditional_losses_627177

inputs
lstm_cell_18_627095
lstm_cell_18_627097
lstm_cell_18_627099
identity??$lstm_cell_18/StatefulPartitionedCall?whileD
ShapeShapeinputs*
T0*
_output_shapes
:2
Shapet
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice/stackx
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_1x
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice/stack_2?
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice\
zeros/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros/mul/yl
	zeros/mulMulstrided_slice:output:0zeros/mul/y:output:0*
T0*
_output_shapes
: 2
	zeros/mul_
zeros/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros/Less/yg

zeros/LessLesszeros/mul:z:0zeros/Less/y:output:0*
T0*
_output_shapes
: 2

zeros/Lessb
zeros/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros/packed/1?
zeros/packedPackstrided_slice:output:0zeros/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros/packed_
zeros/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros/Constu
zerosFillzeros/packed:output:0zeros/Const:output:0*
T0*'
_output_shapes
:?????????2
zeros`
zeros_1/mul/yConst*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/mul/yr
zeros_1/mulMulstrided_slice:output:0zeros_1/mul/y:output:0*
T0*
_output_shapes
: 2
zeros_1/mulc
zeros_1/Less/yConst*
_output_shapes
: *
dtype0*
value
B :?2
zeros_1/Less/yo
zeros_1/LessLesszeros_1/mul:z:0zeros_1/Less/y:output:0*
T0*
_output_shapes
: 2
zeros_1/Lessf
zeros_1/packed/1Const*
_output_shapes
: *
dtype0*
value	B :2
zeros_1/packed/1?
zeros_1/packedPackstrided_slice:output:0zeros_1/packed/1:output:0*
N*
T0*
_output_shapes
:2
zeros_1/packedc
zeros_1/ConstConst*
_output_shapes
: *
dtype0*
valueB
 *    2
zeros_1/Const}
zeros_1Fillzeros_1/packed:output:0zeros_1/Const:output:0*
T0*'
_output_shapes
:?????????2	
zeros_1u
transpose/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose/perm?
	transpose	Transposeinputstranspose/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
	transposeO
Shape_1Shapetranspose:y:0*
T0*
_output_shapes
:2	
Shape_1x
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_1/stack|
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_1|
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_1/stack_2?
strided_slice_1StridedSliceShape_1:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask2
strided_slice_1?
TensorArrayV2/element_shapeConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
TensorArrayV2/element_shape?
TensorArrayV2TensorListReserve$TensorArrayV2/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2?
5TensorArrayUnstack/TensorListFromTensor/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   27
5TensorArrayUnstack/TensorListFromTensor/element_shape?
'TensorArrayUnstack/TensorListFromTensorTensorListFromTensortranspose:y:0>TensorArrayUnstack/TensorListFromTensor/element_shape:output:0*
_output_shapes
: *
element_dtype0*

shape_type02)
'TensorArrayUnstack/TensorListFromTensorx
strided_slice_2/stackConst*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_2/stack|
strided_slice_2/stack_1Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_1|
strided_slice_2/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_2/stack_2?
strided_slice_2StridedSlicetranspose:y:0strided_slice_2/stack:output:0 strided_slice_2/stack_1:output:0 strided_slice_2/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_2?
$lstm_cell_18/StatefulPartitionedCallStatefulPartitionedCallstrided_slice_2:output:0zeros:output:0zeros_1:output:0lstm_cell_18_627095lstm_cell_18_627097lstm_cell_18_627099*
Tin

2*
Tout
2*
_collective_manager_ids
 *M
_output_shapes;
9:?????????:?????????:?????????*%
_read_only_resource_inputs
*-
config_proto

CPU

GPU 2J 8? *Q
fLRJ
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_6267812&
$lstm_cell_18/StatefulPartitionedCall?
TensorArrayV2_1/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   2
TensorArrayV2_1/element_shape?
TensorArrayV2_1TensorListReserve&TensorArrayV2_1/element_shape:output:0strided_slice_1:output:0*
_output_shapes
: *
element_dtype0*

shape_type02
TensorArrayV2_1N
timeConst*
_output_shapes
: *
dtype0*
value	B : 2
time
while/maximum_iterationsConst*
_output_shapes
: *
dtype0*
valueB :
?????????2
while/maximum_iterationsj
while/loop_counterConst*
_output_shapes
: *
dtype0*
value	B : 2
while/loop_counter?
whileWhilewhile/loop_counter:output:0!while/maximum_iterations:output:0time:output:0TensorArrayV2_1:handle:0zeros:output:0zeros_1:output:0strided_slice_1:output:07TensorArrayUnstack/TensorListFromTensor:output_handle:0lstm_cell_18_627095lstm_cell_18_627097lstm_cell_18_627099*
T
2*
_lower_using_switch_merge(*
_num_original_outputs*L
_output_shapes:
8: : : : :?????????:?????????: : : : : *%
_read_only_resource_inputs
	
*
bodyR
while_body_627108*
condR
while_cond_627107*K
output_shapes:
8: : : : :?????????:?????????: : : : : *
parallel_iterations 2
while?
0TensorArrayV2Stack/TensorListStack/element_shapeConst*
_output_shapes
:*
dtype0*
valueB"????   22
0TensorArrayV2Stack/TensorListStack/element_shape?
"TensorArrayV2Stack/TensorListStackTensorListStackwhile:output:39TensorArrayV2Stack/TensorListStack/element_shape:output:0*4
_output_shapes"
 :??????????????????*
element_dtype02$
"TensorArrayV2Stack/TensorListStack?
strided_slice_3/stackConst*
_output_shapes
:*
dtype0*
valueB:
?????????2
strided_slice_3/stack|
strided_slice_3/stack_1Const*
_output_shapes
:*
dtype0*
valueB: 2
strided_slice_3/stack_1|
strided_slice_3/stack_2Const*
_output_shapes
:*
dtype0*
valueB:2
strided_slice_3/stack_2?
strided_slice_3StridedSlice+TensorArrayV2Stack/TensorListStack:tensor:0strided_slice_3/stack:output:0 strided_slice_3/stack_1:output:0 strided_slice_3/stack_2:output:0*
Index0*
T0*'
_output_shapes
:?????????*
shrink_axis_mask2
strided_slice_3y
transpose_1/permConst*
_output_shapes
:*
dtype0*!
valueB"          2
transpose_1/perm?
transpose_1	Transpose+TensorArrayV2Stack/TensorListStack:tensor:0transpose_1/perm:output:0*
T0*4
_output_shapes"
 :??????????????????2
transpose_1f
runtimeConst"/device:CPU:0*
_output_shapes
: *
dtype0*
valueB
 *    2	
runtime?
IdentityIdentitystrided_slice_3:output:0%^lstm_cell_18/StatefulPartitionedCall^while*
T0*'
_output_shapes
:?????????2

Identity"
identityIdentity:output:0*?
_input_shapes.
,:??????????????????:::2L
$lstm_cell_18/StatefulPartitionedCall$lstm_cell_18/StatefulPartitionedCall2
whilewhile:\ X
4
_output_shapes"
 :??????????????????
 
_user_specified_nameinputs"?L
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*?
serving_default?
I
lstm_6_input9
serving_default_lstm_6_input:0?????????x<
dense_180
StatefulPartitionedCall:0?????????tensorflow/serving/predict:??
?/
layer_with_weights-0
layer-0
layer_with_weights-1
layer-1
layer_with_weights-2
layer-2
	optimizer
	variables
regularization_losses
trainable_variables
	keras_api
	
signatures
\_default_save_signature
]__call__
*^&call_and_return_all_conditional_losses"?-
_tf_keras_sequential?,{"class_name": "Sequential", "name": "sequential_12", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "must_restore_from_config": false, "config": {"name": "sequential_12", "layers": [{"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "lstm_6_input"}}, {"class_name": "LSTM", "config": {"name": "lstm_6", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "dtype": "float32", "return_sequences": true, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}, {"class_name": "LSTM", "config": {"name": "lstm_7", "trainable": true, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}, {"class_name": "Dense", "config": {"name": "dense_18", "trainable": true, "dtype": "float32", "units": 24, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}]}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 15]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, 120, 15]}, "is_graph_network": true, "keras_version": "2.4.0", "backend": "tensorflow", "model_config": {"class_name": "Sequential", "config": {"name": "sequential_12", "layers": [{"class_name": "InputLayer", "config": {"batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "dtype": "float32", "sparse": false, "ragged": false, "name": "lstm_6_input"}}, {"class_name": "LSTM", "config": {"name": "lstm_6", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "dtype": "float32", "return_sequences": true, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}, {"class_name": "LSTM", "config": {"name": "lstm_7", "trainable": true, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}, {"class_name": "Dense", "config": {"name": "dense_18", "trainable": true, "dtype": "float32", "units": 24, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}}]}}, "training_config": {"loss": "mae", "metrics": null, "weighted_metrics": null, "loss_weights": null, "optimizer_config": {"class_name": "RMSprop", "config": {"name": "RMSprop", "clipvalue": 1.0, "learning_rate": 0.0010000000474974513, "decay": 0.0, "rho": 0.8999999761581421, "momentum": 0.0, "epsilon": 1e-07, "centered": false}}}}
?

cell

state_spec
	variables
regularization_losses
trainable_variables
	keras_api
___call__
*`&call_and_return_all_conditional_losses"?
_tf_keras_rnn_layer?
{"class_name": "LSTM", "name": "lstm_6", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "stateful": false, "must_restore_from_config": false, "config": {"name": "lstm_6", "trainable": true, "batch_input_shape": {"class_name": "__tuple__", "items": [null, 120, 15]}, "dtype": "float32", "return_sequences": true, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 15]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, 120, 15]}}
?
cell

state_spec
	variables
regularization_losses
trainable_variables
	keras_api
a__call__
*b&call_and_return_all_conditional_losses"?

_tf_keras_rnn_layer?	{"class_name": "LSTM", "name": "lstm_7", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "lstm_7", "trainable": true, "dtype": "float32", "return_sequences": false, "return_state": false, "go_backwards": false, "stateful": false, "unroll": false, "time_major": false, "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}, "input_spec": [{"class_name": "InputSpec", "config": {"dtype": null, "shape": {"class_name": "__tuple__", "items": [null, null, 15]}, "ndim": 3, "max_ndim": null, "min_ndim": null, "axes": {}}}], "build_input_shape": {"class_name": "TensorShape", "items": [null, 120, 15]}}
?

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
c__call__
*d&call_and_return_all_conditional_losses"?
_tf_keras_layer?{"class_name": "Dense", "name": "dense_18", "trainable": true, "expects_training_arg": false, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "dense_18", "trainable": true, "dtype": "float32", "units": 24, "activation": "linear", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "kernel_regularizer": null, "bias_regularizer": null, "activity_regularizer": null, "kernel_constraint": null, "bias_constraint": null}, "input_spec": {"class_name": "InputSpec", "config": {"dtype": null, "shape": null, "ndim": null, "max_ndim": null, "min_ndim": 2, "axes": {"-1": 15}}}, "build_input_shape": {"class_name": "TensorShape", "items": [null, 15]}}
?
iter
	decay
learning_rate
momentum
 rho	rmsT	rmsU	!rmsV	"rmsW	#rmsX	$rmsY	%rmsZ	&rms["
	optimizer
X
!0
"1
#2
$3
%4
&5
6
7"
trackable_list_wrapper
 "
trackable_list_wrapper
X
!0
"1
#2
$3
%4
&5
6
7"
trackable_list_wrapper
?
'metrics
(layer_metrics
)layer_regularization_losses
	variables
regularization_losses

*layers
trainable_variables
+non_trainable_variables
]__call__
\_default_save_signature
*^&call_and_return_all_conditional_losses
&^"call_and_return_conditional_losses"
_generic_user_object
,
eserving_default"
signature_map
?

!kernel
"recurrent_kernel
#bias
,	variables
-trainable_variables
.regularization_losses
/	keras_api
f__call__
*g&call_and_return_all_conditional_losses"?
_tf_keras_layer?{"class_name": "LSTMCell", "name": "lstm_cell_17", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "lstm_cell_17", "trainable": true, "dtype": "float32", "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}
 "
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
 "
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
?
0metrics
1layer_metrics
2layer_regularization_losses
	variables

3states
regularization_losses

4layers
trainable_variables
5non_trainable_variables
___call__
*`&call_and_return_all_conditional_losses
&`"call_and_return_conditional_losses"
_generic_user_object
?

$kernel
%recurrent_kernel
&bias
6	variables
7trainable_variables
8regularization_losses
9	keras_api
h__call__
*i&call_and_return_all_conditional_losses"?
_tf_keras_layer?{"class_name": "LSTMCell", "name": "lstm_cell_18", "trainable": true, "expects_training_arg": true, "dtype": "float32", "batch_input_shape": null, "stateful": false, "must_restore_from_config": false, "config": {"name": "lstm_cell_18", "trainable": true, "dtype": "float32", "units": 15, "activation": "relu", "recurrent_activation": "sigmoid", "use_bias": true, "kernel_initializer": {"class_name": "GlorotUniform", "config": {"seed": null}}, "recurrent_initializer": {"class_name": "Orthogonal", "config": {"gain": 1.0, "seed": null}}, "bias_initializer": {"class_name": "Zeros", "config": {}}, "unit_forget_bias": true, "kernel_regularizer": null, "recurrent_regularizer": null, "bias_regularizer": null, "kernel_constraint": null, "recurrent_constraint": null, "bias_constraint": null, "dropout": 0.0, "recurrent_dropout": 0.0, "implementation": 2}}
 "
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
 "
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
?
:metrics
;layer_metrics
<layer_regularization_losses
	variables

=states
regularization_losses

>layers
trainable_variables
?non_trainable_variables
a__call__
*b&call_and_return_all_conditional_losses
&b"call_and_return_conditional_losses"
_generic_user_object
!:2dense_18/kernel
:2dense_18/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
?
@metrics
Alayer_metrics
Blayer_regularization_losses
	variables
trainable_variables
regularization_losses

Clayers
Dnon_trainable_variables
c__call__
*d&call_and_return_all_conditional_losses
&d"call_and_return_conditional_losses"
_generic_user_object
:	 (2RMSprop/iter
: (2RMSprop/decay
: (2RMSprop/learning_rate
: (2RMSprop/momentum
: (2RMSprop/rho
,:*<2lstm_6/lstm_cell_17/kernel
6:4<2$lstm_6/lstm_cell_17/recurrent_kernel
&:$<2lstm_6/lstm_cell_17/bias
,:*<2lstm_7/lstm_cell_18/kernel
6:4<2$lstm_7/lstm_cell_18/recurrent_kernel
&:$<2lstm_7/lstm_cell_18/bias
'
E0"
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
5
0
1
2"
trackable_list_wrapper
 "
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
5
!0
"1
#2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Fmetrics
Glayer_metrics
Hlayer_regularization_losses
,	variables
-trainable_variables
.regularization_losses

Ilayers
Jnon_trainable_variables
f__call__
*g&call_and_return_all_conditional_losses
&g"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'

0"
trackable_list_wrapper
 "
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
5
$0
%1
&2"
trackable_list_wrapper
 "
trackable_list_wrapper
?
Kmetrics
Llayer_metrics
Mlayer_regularization_losses
6	variables
7trainable_variables
8regularization_losses

Nlayers
Onon_trainable_variables
h__call__
*i&call_and_return_all_conditional_losses
&i"call_and_return_conditional_losses"
_generic_user_object
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
'
0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
?
	Ptotal
	Qcount
R	variables
S	keras_api"?
_tf_keras_metricj{"class_name": "Mean", "name": "loss", "dtype": "float32", "config": {"name": "loss", "dtype": "float32"}}
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
:  (2total
:  (2count
.
P0
Q1"
trackable_list_wrapper
-
R	variables"
_generic_user_object
+:)2RMSprop/dense_18/kernel/rms
%:#2RMSprop/dense_18/bias/rms
6:4<2&RMSprop/lstm_6/lstm_cell_17/kernel/rms
@:><20RMSprop/lstm_6/lstm_cell_17/recurrent_kernel/rms
0:.<2$RMSprop/lstm_6/lstm_cell_17/bias/rms
6:4<2&RMSprop/lstm_7/lstm_cell_18/kernel/rms
@:><20RMSprop/lstm_7/lstm_cell_18/recurrent_kernel/rms
0:.<2$RMSprop/lstm_7/lstm_cell_18/bias/rms
?2?
!__inference__wrapped_model_626098?
???
FullArgSpec
args? 
varargsjargs
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? */?,
*?'
lstm_6_input?????????x
?2?
.__inference_sequential_12_layer_call_fn_628087
.__inference_sequential_12_layer_call_fn_628131
.__inference_sequential_12_layer_call_fn_628820
.__inference_sequential_12_layer_call_fn_628799?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628470
I__inference_sequential_12_layer_call_and_return_conditional_losses_628019
I__inference_sequential_12_layer_call_and_return_conditional_losses_628778
I__inference_sequential_12_layer_call_and_return_conditional_losses_628042?
???
FullArgSpec1
args)?&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults?
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
'__inference_lstm_6_layer_call_fn_629148
'__inference_lstm_6_layer_call_fn_629476
'__inference_lstm_6_layer_call_fn_629137
'__inference_lstm_6_layer_call_fn_629465?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
B__inference_lstm_6_layer_call_and_return_conditional_losses_628973
B__inference_lstm_6_layer_call_and_return_conditional_losses_629454
B__inference_lstm_6_layer_call_and_return_conditional_losses_629126
B__inference_lstm_6_layer_call_and_return_conditional_losses_629301?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
'__inference_lstm_7_layer_call_fn_629804
'__inference_lstm_7_layer_call_fn_629793
'__inference_lstm_7_layer_call_fn_630132
'__inference_lstm_7_layer_call_fn_630121?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629629
B__inference_lstm_7_layer_call_and_return_conditional_losses_629782
B__inference_lstm_7_layer_call_and_return_conditional_losses_630110
B__inference_lstm_7_layer_call_and_return_conditional_losses_629957?
???
FullArgSpecB
args:?7
jself
jinputs
jmask

jtraining
jinitial_state
varargs
 
varkw
 
defaults?

 
p 

 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
)__inference_dense_18_layer_call_fn_630151?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
D__inference_dense_18_layer_call_and_return_conditional_losses_630142?
???
FullArgSpec
args?
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?B?
$__inference_signature_wrapper_628162lstm_6_input"?
???
FullArgSpec
args? 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs? 
kwonlydefaults
 
annotations? *
 
?2?
-__inference_lstm_cell_17_layer_call_fn_630234
-__inference_lstm_cell_17_layer_call_fn_630251?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630217
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630184?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
-__inference_lstm_cell_18_layer_call_fn_630351
-__inference_lstm_cell_18_layer_call_fn_630334?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 
?2?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630317
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630284?
???
FullArgSpec3
args+?(
jself
jinputs
jstates

jtraining
varargs
 
varkw
 
defaults?
p 

kwonlyargs? 
kwonlydefaults? 
annotations? *
 ?
!__inference__wrapped_model_626098z!"#$%&9?6
/?,
*?'
lstm_6_input?????????x
? "3?0
.
dense_18"?
dense_18??????????
D__inference_dense_18_layer_call_and_return_conditional_losses_630142\/?,
%?"
 ?
inputs?????????
? "%?"
?
0?????????
? |
)__inference_dense_18_layer_call_fn_630151O/?,
%?"
 ?
inputs?????????
? "???????????
B__inference_lstm_6_layer_call_and_return_conditional_losses_628973?!"#O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p

 
? "2?/
(?%
0??????????????????
? ?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629126?!"#O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p 

 
? "2?/
(?%
0??????????????????
? ?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629301q!"#??<
5?2
$?!
inputs?????????x

 
p

 
? ")?&
?
0?????????x
? ?
B__inference_lstm_6_layer_call_and_return_conditional_losses_629454q!"#??<
5?2
$?!
inputs?????????x

 
p 

 
? ")?&
?
0?????????x
? ?
'__inference_lstm_6_layer_call_fn_629137}!"#O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p

 
? "%?"???????????????????
'__inference_lstm_6_layer_call_fn_629148}!"#O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p 

 
? "%?"???????????????????
'__inference_lstm_6_layer_call_fn_629465d!"#??<
5?2
$?!
inputs?????????x

 
p

 
? "??????????x?
'__inference_lstm_6_layer_call_fn_629476d!"#??<
5?2
$?!
inputs?????????x

 
p 

 
? "??????????x?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629629}$%&O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p

 
? "%?"
?
0?????????
? ?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629782}$%&O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p 

 
? "%?"
?
0?????????
? ?
B__inference_lstm_7_layer_call_and_return_conditional_losses_629957m$%&??<
5?2
$?!
inputs?????????x

 
p

 
? "%?"
?
0?????????
? ?
B__inference_lstm_7_layer_call_and_return_conditional_losses_630110m$%&??<
5?2
$?!
inputs?????????x

 
p 

 
? "%?"
?
0?????????
? ?
'__inference_lstm_7_layer_call_fn_629793p$%&O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p

 
? "???????????
'__inference_lstm_7_layer_call_fn_629804p$%&O?L
E?B
4?1
/?,
inputs/0??????????????????

 
p 

 
? "???????????
'__inference_lstm_7_layer_call_fn_630121`$%&??<
5?2
$?!
inputs?????????x

 
p

 
? "???????????
'__inference_lstm_7_layer_call_fn_630132`$%&??<
5?2
$?!
inputs?????????x

 
p 

 
? "???????????
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630184?!"#??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p
? "s?p
i?f
?
0/0?????????
E?B
?
0/1/0?????????
?
0/1/1?????????
? ?
H__inference_lstm_cell_17_layer_call_and_return_conditional_losses_630217?!"#??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p 
? "s?p
i?f
?
0/0?????????
E?B
?
0/1/0?????????
?
0/1/1?????????
? ?
-__inference_lstm_cell_17_layer_call_fn_630234?!"#??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p
? "c?`
?
0?????????
A?>
?
1/0?????????
?
1/1??????????
-__inference_lstm_cell_17_layer_call_fn_630251?!"#??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p 
? "c?`
?
0?????????
A?>
?
1/0?????????
?
1/1??????????
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630284?$%&??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p
? "s?p
i?f
?
0/0?????????
E?B
?
0/1/0?????????
?
0/1/1?????????
? ?
H__inference_lstm_cell_18_layer_call_and_return_conditional_losses_630317?$%&??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p 
? "s?p
i?f
?
0/0?????????
E?B
?
0/1/0?????????
?
0/1/1?????????
? ?
-__inference_lstm_cell_18_layer_call_fn_630334?$%&??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p
? "c?`
?
0?????????
A?>
?
1/0?????????
?
1/1??????????
-__inference_lstm_cell_18_layer_call_fn_630351?$%&??}
v?s
 ?
inputs?????????
K?H
"?
states/0?????????
"?
states/1?????????
p 
? "c?`
?
0?????????
A?>
?
1/0?????????
?
1/1??????????
I__inference_sequential_12_layer_call_and_return_conditional_losses_628019t!"#$%&A?>
7?4
*?'
lstm_6_input?????????x
p

 
? "%?"
?
0?????????
? ?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628042t!"#$%&A?>
7?4
*?'
lstm_6_input?????????x
p 

 
? "%?"
?
0?????????
? ?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628470n!"#$%&;?8
1?.
$?!
inputs?????????x
p

 
? "%?"
?
0?????????
? ?
I__inference_sequential_12_layer_call_and_return_conditional_losses_628778n!"#$%&;?8
1?.
$?!
inputs?????????x
p 

 
? "%?"
?
0?????????
? ?
.__inference_sequential_12_layer_call_fn_628087g!"#$%&A?>
7?4
*?'
lstm_6_input?????????x
p

 
? "???????????
.__inference_sequential_12_layer_call_fn_628131g!"#$%&A?>
7?4
*?'
lstm_6_input?????????x
p 

 
? "???????????
.__inference_sequential_12_layer_call_fn_628799a!"#$%&;?8
1?.
$?!
inputs?????????x
p

 
? "???????????
.__inference_sequential_12_layer_call_fn_628820a!"#$%&;?8
1?.
$?!
inputs?????????x
p 

 
? "???????????
$__inference_signature_wrapper_628162?!"#$%&I?F
? 
??<
:
lstm_6_input*?'
lstm_6_input?????????x"3?0
.
dense_18"?
dense_18?????????