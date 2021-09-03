use crate::{
    nodes::traits::Result,
    tokenizer::{
        whitespace_parser::{parse_parenthesizable_whitespace, Config},
        Token,
    },
    Param, Parameters, StarArg,
};

pub(crate) fn adjust_parameters_trailing_whitespace<'a>(
    config: &Config<'a>,
    parameters: &mut Parameters<'a>,
    next_tok: &mut Token<'a>,
) -> Result<()> {
    let mut do_adjust = |param: &mut Param<'a>| -> Result<()> {
        let whitespace_after =
            parse_parenthesizable_whitespace(config, &mut next_tok.whitespace_before.borrow_mut())?;
        if param.comma.is_none() {
            param.whitespace_after_param = whitespace_after;
        }
        Ok(())
    };

    if let Some(param) = &mut parameters.star_kwarg {
        do_adjust(param)?;
    } else if let Some(param) = parameters.kwonly_params.last_mut() {
        do_adjust(param)?;
    } else if let Some(StarArg::Param(param)) = parameters.star_arg.as_mut() {
        do_adjust(param)?;
    } else if let Some(param) = parameters.params.last_mut() {
        do_adjust(param)?;
    }
    Ok(())
}
