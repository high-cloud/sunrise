# sunrise

yyd的语言实现练习

## 首先从最简单的开始实现
1. 最基本的lambda演算
2. 实现llvm后端

## code gen
1. 以llvm为后端
想想整个过程，必然从Program,`Binds`开始。
所以要处理每个bind

分解成处理lambdaFrom
要做：
1. 全局变量区（或者直接在heap上）申请一个node，分别把node的基本
